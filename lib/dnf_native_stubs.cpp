#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <libdnf5/base/base.hpp>
#include <libdnf5/comps/environment/environment.hpp>
#include <libdnf5/comps/environment/query.hpp>
#include <libdnf5/comps/group/group.hpp>
#include <libdnf5/comps/group/query.hpp>
#include <libdnf5/conf/const.hpp>
#include <libdnf5/repo/repo_sack.hpp>
#include <libdnf5/rpm/package_query.hpp>

#include <exception>
#include <string>
#include <utility>
#include <vector>

namespace {

struct snapshot_data {
    std::vector<std::pair<std::string, int>> packages;
    std::vector<std::pair<std::string, std::vector<std::string>>> groups;
    std::vector<std::pair<std::string, std::vector<std::string>>> environments;
    std::vector<std::string> installed_groups;
    std::vector<std::string> installed_environments;
};

// Tag values are matched by Dnf_plan.Reason.of_tag.
int reason_tag(libdnf5::transaction::TransactionItemReason reason) {
    using Reason = libdnf5::transaction::TransactionItemReason;
    switch (reason) {
        case Reason::NONE:
            return 0;
        case Reason::DEPENDENCY:
            return 1;
        case Reason::USER:
            return 2;
        case Reason::CLEAN:
            return 3;
        case Reason::WEAK_DEPENDENCY:
            return 4;
        case Reason::GROUP:
            return 5;
        case Reason::EXTERNAL_USER:
            return 6;
    }
    return -1;
}

void setup_base(libdnf5::Base & base, bool with_available, bool cacheonly) {
    base.load_config();
    auto & config = base.get_config();
    if (cacheonly) {
        config.get_cacheonly_option().set(libdnf5::Option::Priority::RUNTIME, "all");
    }
    if (with_available) {
        config.get_optional_metadata_types_option().add_item(
            libdnf5::Option::Priority::RUNTIME, libdnf5::METADATA_TYPE_COMPS);
    }
    base.setup();
    auto repo_sack = base.get_repo_sack();
    repo_sack->create_repos_from_system_configuration();
    if (with_available) {
        repo_sack->load_repos();
    } else {
        repo_sack->load_repos(libdnf5::repo::Repo::Type::SYSTEM);
    }
}

std::string collect_snapshot(bool cacheonly, snapshot_data & out) {
    try {
        libdnf5::Base base;
        setup_base(base, true, cacheonly);
        {
            libdnf5::rpm::PackageQuery query(base, libdnf5::sack::ExcludeFlags::IGNORE_EXCLUDES);
            query.filter_installed();
            for (const auto & package : query) {
                out.packages.emplace_back(package.get_name(), reason_tag(package.get_reason()));
            }
        }
        {
            libdnf5::comps::GroupQuery query(base);
            query.filter_installed(false);
            for (auto group : query.list()) {
                std::vector<std::string> members;
                auto installable = libdnf5::comps::PackageType::MANDATORY |
                                   libdnf5::comps::PackageType::DEFAULT |
                                   libdnf5::comps::PackageType::CONDITIONAL;
                for (auto & package : group.get_packages_of_type(installable)) {
                    members.push_back(package.get_name());
                }
                out.groups.emplace_back(group.get_groupid(), std::move(members));
            }
        }
        {
            libdnf5::comps::GroupQuery query(base);
            query.filter_installed(true);
            for (auto group : query.list()) {
                out.installed_groups.push_back(group.get_groupid());
            }
        }
        {
            libdnf5::comps::EnvironmentQuery query(base);
            query.filter_installed(false);
            for (auto environment : query.list()) {
                out.environments.emplace_back(environment.get_environmentid(), environment.get_groups());
            }
        }
        {
            libdnf5::comps::EnvironmentQuery query(base);
            query.filter_installed(true);
            for (auto environment : query.list()) {
                out.installed_environments.push_back(environment.get_environmentid());
            }
        }
        return "";
    } catch (const std::exception & ex) {
        return std::string("libdnf5: ") + ex.what();
    } catch (...) {
        return "libdnf5: unknown error";
    }
}

std::string collect_unneeded(
    std::vector<std::string> & removable,
    std::vector<std::string> & protected_packages) {
    try {
        libdnf5::Base base;
        setup_base(base, false, false);
        libdnf5::rpm::PackageQuery unneeded(base);
        unneeded.filter_unneeded();
        libdnf5::rpm::PackageQuery protected_query(base, libdnf5::sack::ExcludeFlags::IGNORE_EXCLUDES);
        protected_query.filter_installed();
        protected_query.filter_name(base.get_config().get_protected_packages_option().get_value());
        for (const auto & package : unneeded) {
            if (protected_query.contains(package)) {
                protected_packages.push_back(package.get_name());
            } else {
                removable.push_back(package.get_name());
            }
        }
        return "";
    } catch (const std::exception & ex) {
        return std::string("libdnf5: ") + ex.what();
    } catch (...) {
        return "libdnf5: unknown error";
    }
}

value string_list(const std::vector<std::string> & items) {
    CAMLparam0();
    CAMLlocal3(list, cell, str);
    list = Val_emptylist;
    for (auto it = items.rbegin(); it != items.rend(); ++it) {
        str = caml_copy_string(it->c_str());
        cell = caml_alloc(2, 0);
        Store_field(cell, 0, str);
        Store_field(cell, 1, list);
        list = cell;
    }
    CAMLreturn(list);
}

value string_int_pair_list(const std::vector<std::pair<std::string, int>> & items) {
    CAMLparam0();
    CAMLlocal4(list, cell, pair, str);
    list = Val_emptylist;
    for (auto it = items.rbegin(); it != items.rend(); ++it) {
        str = caml_copy_string(it->first.c_str());
        pair = caml_alloc_tuple(2);
        Store_field(pair, 0, str);
        Store_field(pair, 1, Val_int(it->second));
        cell = caml_alloc(2, 0);
        Store_field(cell, 0, pair);
        Store_field(cell, 1, list);
        list = cell;
    }
    CAMLreturn(list);
}

value string_list_pair_list(const std::vector<std::pair<std::string, std::vector<std::string>>> & items) {
    CAMLparam0();
    CAMLlocal5(list, cell, pair, str, members);
    list = Val_emptylist;
    for (auto it = items.rbegin(); it != items.rend(); ++it) {
        str = caml_copy_string(it->first.c_str());
        members = string_list(it->second);
        pair = caml_alloc_tuple(2);
        Store_field(pair, 0, str);
        Store_field(pair, 1, members);
        cell = caml_alloc(2, 0);
        Store_field(cell, 0, pair);
        Store_field(cell, 1, list);
        list = cell;
    }
    CAMLreturn(list);
}

// Field order matches Dnf_native.Raw.t.
value snapshot_value(const snapshot_data & data) {
    CAMLparam0();
    CAMLlocal2(record, field);
    record = caml_alloc_tuple(5);
    field = string_int_pair_list(data.packages);
    Store_field(record, 0, field);
    field = string_list_pair_list(data.groups);
    Store_field(record, 1, field);
    field = string_list_pair_list(data.environments);
    Store_field(record, 2, field);
    field = string_list(data.installed_groups);
    Store_field(record, 3, field);
    field = string_list(data.installed_environments);
    Store_field(record, 4, field);
    CAMLreturn(record);
}

value result_value(const std::string & error, value payload) {
    CAMLparam1(payload);
    CAMLlocal2(result, message);
    if (error.empty()) {
        result = caml_alloc(1, 0);
        Store_field(result, 0, payload);
    } else {
        message = caml_copy_string(error.c_str());
        result = caml_alloc(1, 1);
        Store_field(result, 0, message);
    }
    CAMLreturn(result);
}

}  // namespace

extern "C" value gkt_dnf5_snapshot(value v_cacheonly) {
    CAMLparam1(v_cacheonly);
    CAMLlocal1(payload);
    bool cacheonly = Bool_val(v_cacheonly);
    snapshot_data data;
    std::string error;
    caml_release_runtime_system();
    error = collect_snapshot(cacheonly, data);
    caml_acquire_runtime_system();
    payload = error.empty() ? snapshot_value(data) : Val_unit;
    CAMLreturn(result_value(error, payload));
}

extern "C" value gkt_dnf5_unneeded(value v_unit) {
    CAMLparam1(v_unit);
    CAMLlocal3(payload, removable_value, protected_value);
    std::vector<std::string> removable;
    std::vector<std::string> protected_packages;
    std::string error;
    caml_release_runtime_system();
    error = collect_unneeded(removable, protected_packages);
    caml_acquire_runtime_system();
    if (error.empty()) {
        removable_value = string_list(removable);
        protected_value = string_list(protected_packages);
        payload = caml_alloc_tuple(2);
        Store_field(payload, 0, removable_value);
        Store_field(payload, 1, protected_value);
    } else {
        payload = Val_unit;
    }
    CAMLreturn(result_value(error, payload));
}
