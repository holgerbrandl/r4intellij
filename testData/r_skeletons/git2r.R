##
## Exported symobls in package `git2r`
##

## Exported package methods

is_head <- function (branch) 
standardGeneric("is_head")


tag_delete <- function (object, ...) 
standardGeneric("tag_delete")


stash_list <- function (repo) 
standardGeneric("stash_list")


lookup <- function (repo, sha) 
standardGeneric("lookup")


`.__T__[:base` <- "<environment>"

libgit2_version <- function () 
{
    .Call(git2r_libgit2_version)
}


`.__T__commit:git2r` <- "<environment>"

.__C__git_note <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    annotated = structure("character", package = "methods"), 
    message = structure("character", package = "methods"), refname = structure("character", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"annotated", "message", "refname", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_note", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


branches <- function (repo, flags = c("all", "local", "remote")) 
standardGeneric("branches")


discover_repository <- function (path, ceiling) 
standardGeneric("discover_repository")


blame <- function (repo, path) 
standardGeneric("blame")


`.__T__tree:git2r` <- "<environment>"

pull <- function (repo, credentials = NULL, merger = default_signature(repo)) 
standardGeneric("pull")


`.__T__blob_create:git2r` <- "<environment>"

remote_rename <- function (repo, oldname, newname) 
standardGeneric("remote_rename")


`.__T__pull:git2r` <- "<environment>"

repository <- function (path, ...) 
standardGeneric("repository")


`.__T__branch_remote_name:git2r` <- "<environment>"

references <- function (repo) 
standardGeneric("references")


`.__T__remote_rename:git2r` <- "<environment>"

.__C__git_tag <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    message = structure("character", package = "methods"), name = structure("character", package = "methods"), 
    tagger = structure("git_signature", package = "git2r"), target = structure("character", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"message", "name", "tagger", "target", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- validObject(object@tagger)
    if (identical(errors, TRUE)) 
        errors <- character()
    if (!identical(length(object@sha), 1L)) 
        errors <- c(errors, "sha must have length equal to one")
    if (!identical(length(object@message), 1L)) 
        errors <- c(errors, "message must have length equal to one")
    if (!identical(length(object@name), 1L)) 
        errors <- c(errors, "name must have length equal to one")
    if (!identical(length(object@target), 1L)) 
        errors <- c(errors, "target must have length equal to one")
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_tag", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__plot:graphics` <- "<environment>"

.__C__cred_user_pass <- new("classRepresentation"
    , slots = structure(list(username = structure("character", package = "methods"), 
    password = structure("character", package = "methods")), .Names = c("username", 
"password"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("cred_user_pass", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


diff <- function (x, ...) 
standardGeneric("diff")


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

.__C__cred_env <- new("classRepresentation"
    , slots = structure(list(username = structure("character", package = "methods"), 
    password = structure("character", package = "methods")), .Names = c("username", 
"password"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("cred_env", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__discover_repository:git2r` <- "<environment>"

branch_rename <- function (branch, name, force = FALSE) 
standardGeneric("branch_rename")


is_commit <- function (object) 
{
    is(object = object, class2 = "git_commit")
}


`.__T__repository:git2r` <- "<environment>"

`.__T__odb_blobs:git2r` <- "<environment>"

.__C__git_blame_hunk <- new("classRepresentation"
    , slots = structure(list(lines_in_hunk = structure("integer", package = "methods"), 
    final_commit_id = structure("character", package = "methods"), 
    final_start_line_number = structure("integer", package = "methods"), 
    final_signature = structure("git_signature", package = "git2r"), 
    orig_commit_id = structure("character", package = "methods"), 
    orig_start_line_number = structure("integer", package = "methods"), 
    orig_signature = structure("git_signature", package = "git2r"), 
    orig_path = structure("character", package = "methods"), 
    boundary = structure("logical", package = "methods"), repo = structure("git_repository", package = "git2r")), .Names = c("lines_in_hunk", 
"final_commit_id", "final_start_line_number", "final_signature", 
"orig_commit_id", "orig_start_line_number", "orig_signature", 
"orig_path", "boundary", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- character()
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_blame_hunk", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__remote_url:git2r` <- "<environment>"

`.__T__remote_remove:git2r` <- "<environment>"

hash <- function (data) 
standardGeneric("hash")


`.__T__init:git2r` <- "<environment>"

.__C__git_reflog_entry <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    message = structure("character", package = "methods"), index = structure("integer", package = "methods"), 
    committer = structure("git_signature", package = "git2r"), 
    refname = structure("character", package = "methods"), repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"message", "index", "committer", "refname", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_reflog_entry", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


is_binary <- function (blob) 
standardGeneric("is_binary")


`.__T__branch_delete:git2r` <- "<environment>"

`.__T__workdir:git2r` <- "<environment>"

`.__T__branch_target:git2r` <- "<environment>"

remotes <- function (repo) 
standardGeneric("remotes")


`.__T__fetch:git2r` <- "<environment>"

`.__T__merge:base` <- "<environment>"

remote_set_url <- function (repo, name, url) 
standardGeneric("remote_set_url")


commits <- function (repo, ...) 
standardGeneric("commits")


workdir <- function (repo) 
standardGeneric("workdir")


`.__T__note_create:git2r` <- "<environment>"

`.__T__references:git2r` <- "<environment>"

tags <- function (repo) 
standardGeneric("tags")


is_merge <- function (commit) 
standardGeneric("is_merge")


`.__T__checkout:git2r` <- "<environment>"

is_shallow <- function (repo) 
standardGeneric("is_shallow")


`.__T__merge_base:git2r` <- "<environment>"

`.__T__blame:git2r` <- "<environment>"

.__C__cred_token <- new("classRepresentation"
    , slots = structure(list(token = structure("character", package = "methods")), .Names = "token")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("cred_token", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__git_blob <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- character()
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_blob", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__note_remove:git2r` <- "<environment>"

`.__T__hash:git2r` <- "<environment>"

add <- function (repo, path, ...) 
standardGeneric("add")


index_remove_bypath <- function (repo, path) 
standardGeneric("index_remove_bypath")


.__C__git_diff <- new("classRepresentation"
    , slots = structure(list(old = structure("ANY", package = "methods"), new = structure("ANY", package = "methods"), 
    files = structure("list", package = "methods")), .Names = c("old", 
"new", "files"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_diff", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

bundle_r_package <- function (repo) 
standardGeneric("bundle_r_package")


`.__T__status:git2r` <- "<environment>"

show <- methods::show # re-exported from methods package

.__C__git_branch <- new("classRepresentation"
    , slots = structure(list(name = structure("character", package = "methods"), 
    type = structure("integer", package = "methods"), repo = structure("git_repository", package = "git2r")), .Names = c("name", 
"type", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_branch", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


branch_get_upstream <- function (branch) 
standardGeneric("branch_get_upstream")


in_repository <- function (path) 
standardGeneric("in_repository")


`.__T__is_empty:git2r` <- "<environment>"

`.__T__lookup:git2r` <- "<environment>"

`.__T__push:git2r` <- "<environment>"

hashfile <- function (path) 
standardGeneric("hashfile")


.__C__git_commit <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    author = structure("git_signature", package = "git2r"), committer = structure("git_signature", package = "git2r"), 
    summary = structure("character", package = "methods"), message = structure("character", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"author", "committer", "summary", "message", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_commit", package = "git2r")
    , package = "git2r"
    , subclasses = structure(list(git_stash = S4_object()), .Names = "git_stash")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


branch_create <- function (commit, name, force = FALSE) 
standardGeneric("branch_create")


`.__T__head:utils` <- "<environment>"

summary <- function (object, ...) 
standardGeneric("summary")


remote_add <- function (repo, name, url) 
standardGeneric("remote_add")


`.__T__stash:git2r` <- "<environment>"

`.__T__parents:git2r` <- "<environment>"

`.__T__is_binary:git2r` <- "<environment>"

commit <- function (repo, message = NULL, all = FALSE, session = FALSE, 
    reference = "HEAD", author = default_signature(repo), committer = default_signature(repo)) 
standardGeneric("commit")


clone <- function (url, local_path, bare = FALSE, branch = NULL, credentials = NULL, 
    progress = TRUE) 
standardGeneric("clone")


branch_remote_url <- function (branch) 
standardGeneric("branch_remote_url")


.__C__git_transfer_progress <- new("classRepresentation"
    , slots = structure(list(total_objects = structure("integer", package = "methods"), 
    indexed_objects = structure("integer", package = "methods"), 
    received_objects = structure("integer", package = "methods"), 
    local_objects = structure("integer", package = "methods"), 
    total_deltas = structure("integer", package = "methods"), 
    indexed_deltas = structure("integer", package = "methods"), 
    received_bytes = structure("integer", package = "methods")), .Names = c("total_objects", 
"indexed_objects", "received_objects", "local_objects", "total_deltas", 
"indexed_deltas", "received_bytes"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_transfer_progress", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


is_local <- function (branch) 
standardGeneric("is_local")


`.__T__remotes:git2r` <- "<environment>"

`.__T__stash_list:git2r` <- "<environment>"

`.__T__commits:git2r` <- "<environment>"

remote_ls <- function (name, repo = NULL, credentials = NULL) 
standardGeneric("remote_ls")


`.__T__cred_env:git2r` <- "<environment>"

libgit2_sha <- function () 
"75db289a041b1f1084768244e167b953ac7eeaa5"


is_branch <- function (object) 
{
    methods::is(object = object, class2 = "git_branch")
}


.__C__git_diff_line <- new("classRepresentation"
    , slots = structure(list(origin = structure("integer", package = "methods"), 
    old_lineno = structure("integer", package = "methods"), new_lineno = structure("integer", package = "methods"), 
    num_lines = structure("integer", package = "methods"), content = structure("character", package = "methods")), .Names = c("origin", 
"old_lineno", "new_lineno", "num_lines", "content"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_diff_line", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


ahead_behind <- function (local, upstream) 
standardGeneric("ahead_behind")


note_remove <- function (note, author = default_signature(note@repo), committer = default_signature(note@repo)) 
standardGeneric("note_remove")


`.__T__is_head:git2r` <- "<environment>"

note_create <- function (object, message, ref = note_default_ref(object@repo), 
    author = default_signature(object@repo), committer = default_signature(object@repo), 
    force = FALSE) 
standardGeneric("note_create")


parents <- function (object) 
standardGeneric("parents")


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

tag <- function (object, name, message, session = FALSE, tagger = default_signature(object)) 
standardGeneric("tag")


libgit2_features <- function () 
{
    .Call(git2r_libgit2_features)
}


notes <- function (repo, ref = note_default_ref(repo)) 
standardGeneric("notes")


odb_objects <- function (repo) 
standardGeneric("odb_objects")


.__C__git_reference <- new("classRepresentation"
    , slots = structure(list(name = structure("character", package = "methods"), 
    type = structure("integer", package = "methods"), sha = structure("character", package = "methods"), 
    target = structure("character", package = "methods"), shorthand = structure("character", package = "methods")), .Names = c("name", 
"type", "sha", "target", "shorthand"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_reference", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__remote_set_url:git2r` <- "<environment>"

push <- function (object, ...) 
standardGeneric("push")


`.__T__branches:git2r` <- "<environment>"

`.__T__in_repository:git2r` <- "<environment>"

.__C__git_signature <- new("classRepresentation"
    , slots = structure(list(name = structure("character", package = "methods"), 
    email = structure("character", package = "methods"), when = structure("git_time", package = "git2r")), .Names = c("name", 
"email", "when"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- validObject(object@when)
    if (identical(errors, TRUE)) 
        errors <- character()
    if (!identical(length(object@name), 1L)) 
        errors <- c(errors, "name must have length equal to one")
    if (!identical(length(object@email), 1L)) 
        errors <- c(errors, "email must have length equal to one")
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_signature", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


branch_remote_name <- function (branch) 
standardGeneric("branch_remote_name")


note_default_ref <- function (repo) 
standardGeneric("note_default_ref")


reflog <- function (repo, refname) 
standardGeneric("reflog")


branch_set_upstream <- function (branch, name) 
standardGeneric("branch_set_upstream")


branch_delete <- function (branch) 
standardGeneric("branch_delete")


`.__T__length:base` <- "<environment>"

`.__T__when:git2r` <- "<environment>"

`.__T__punch_card:git2r` <- "<environment>"

`.__T__summary:base` <- "<environment>"

`.__T__diff:base` <- "<environment>"

`.__T__contributions:git2r` <- "<environment>"

cred_user_pass <- function (username, password) 
standardGeneric("cred_user_pass")


status <- function (repo, staged = TRUE, unstaged = TRUE, untracked = TRUE, 
    ignored = FALSE, all_untracked = FALSE, ...) 
standardGeneric("status")


`.__T__is_detached:git2r` <- "<environment>"

`.__T__branch_remote_url:git2r` <- "<environment>"

.__C__git_stash <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    author = structure("git_signature", package = "git2r"), committer = structure("git_signature", package = "git2r"), 
    summary = structure("character", package = "methods"), message = structure("character", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"author", "committer", "summary", "message", "repo"))
    , contains = structure(list(git_commit = S4_object()), .Names = "git_commit")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_stash", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


punch_card <- function (repo, main = NULL, ...) 
standardGeneric("punch_card")


descendant_of <- function (commit, ancestor) 
standardGeneric("descendant_of")


`.__T__show:methods` <- "<environment>"

default_signature <- function (repo) 
standardGeneric("default_signature")


`.__T__stash_drop:git2r` <- "<environment>"

content <- function (blob, split = TRUE) 
standardGeneric("content")


blob_create <- function (repo, path, relative = TRUE) 
standardGeneric("blob_create")


`.__T__tag_delete:git2r` <- "<environment>"

ssl_cert_locations <- function (filename = NULL, path = NULL) 
{
    invisible(.Call(git2r_ssl_cert_locations, filename, path))
}


`.__T__is_local:git2r` <- "<environment>"

plot <- graphics::plot # re-exported from graphics package

remote_url <- function (repo, remote = remotes(repo)) 
standardGeneric("remote_url")


stash <- function (object, message = as.character(Sys.time()), index = FALSE, 
    untracked = FALSE, ignored = FALSE, stasher = default_signature(object)) 
standardGeneric("stash")


.__C__cred_ssh_key <- new("classRepresentation"
    , slots = structure(list(publickey = structure("character", package = "methods"), 
    privatekey = structure("character", package = "methods"), 
    passphrase = structure("character", package = "methods")), .Names = c("publickey", 
"privatekey", "passphrase"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("cred_ssh_key", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


init <- function (path, bare = FALSE) 
standardGeneric("init")


.__C__git_tree <- new("classRepresentation"
    , slots = structure(list(sha = structure("character", package = "methods"), 
    filemode = structure("integer", package = "methods"), type = structure("character", package = "methods"), 
    id = structure("character", package = "methods"), name = structure("character", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("sha", 
"filemode", "type", "id", "name", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- character(0)
    if (!identical(length(object@sha), 1L)) 
        errors <- c(errors, "sha must have length equal to one")
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_tree", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


reset <- function (object, ...) 
standardGeneric("reset")


`.__T__bundle_r_package:git2r` <- "<environment>"

cred_token <- function (token = "GITHUB_PAT") 
{
    new("cred_token", token = token)
}


`.__T__content:git2r` <- "<environment>"

odb_blobs <- function (repo) 
standardGeneric("odb_blobs")


revparse_single <- function (repo, revision) 
standardGeneric("revparse_single")


`.__T__fetch_heads:git2r` <- "<environment>"

cred_ssh_key <- function (publickey = "~/.ssh/id_rsa.pub", privatekey = "~/.ssh/id_rsa", 
    passphrase = character(0)) 
{
    publickey = normalizePath(publickey, mustWork = TRUE)
    privatekey = normalizePath(privatekey, mustWork = TRUE)
    if (length(passphrase) == 0) {
        if (ssh_key_needs_passphrase(privatekey)) {
            if (requireNamespace("getPass", quietly = TRUE)) {
                passphrase <- getPass::getPass()
            }
        }
    }
    new("cred_ssh_key", publickey = publickey, privatekey = privatekey, 
        passphrase = passphrase)
}


`.__T__index_remove_bypath:git2r` <- "<environment>"

`.__T__note_default_ref:git2r` <- "<environment>"

`.__T__default_signature:git2r` <- "<environment>"

`.__T__remote_add:git2r` <- "<environment>"

`.__T__add:git2r` <- "<environment>"

when <- function (object) 
standardGeneric("when")


.__C__git_merge_result <- new("classRepresentation"
    , slots = structure(list(up_to_date = structure("logical", package = "methods"), 
    fast_forward = structure("logical", package = "methods"), 
    conflicts = structure("logical", package = "methods"), sha = structure("character", package = "methods")), .Names = c("up_to_date", 
"fast_forward", "conflicts", "sha"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_merge_result", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__rm_file:git2r` <- "<environment>"

.__C__git_time <- new("classRepresentation"
    , slots = structure(list(time = structure("numeric", package = "methods"), 
    offset = structure("numeric", package = "methods")), .Names = c("time", 
"offset"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- character()
    if (!identical(length(object@time), 1L)) 
        errors <- c(errors, "time must have length equal to one")
    if (!identical(length(object@offset), 1L)) 
        errors <- c(errors, "offset must have length equal to one")
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_time", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


merge_base <- function (one, two) 
standardGeneric("merge_base")


`.__T__branch_create:git2r` <- "<environment>"

`.__T__notes:git2r` <- "<environment>"

head <- utils::head # re-exported from utils package

is_bare <- function (repo) 
standardGeneric("is_bare")


cred_env <- function (username, password) 
standardGeneric("cred_env")


`.__T__tag:git2r` <- "<environment>"

`.__T__revparse_single:git2r` <- "<environment>"

.__C__git_diff_file <- new("classRepresentation"
    , slots = structure(list(old_file = structure("character", package = "methods"), 
    new_file = structure("character", package = "methods"), hunks = structure("list", package = "methods")), .Names = c("old_file", 
"new_file", "hunks"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_diff_file", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


is_empty <- function (repo) 
standardGeneric("is_empty")


`.__T__branch_rename:git2r` <- "<environment>"

rm_file <- function (repo, path) 
standardGeneric("rm_file")


`.__T__cred_user_pass:git2r` <- "<environment>"

branch_target <- function (branch) 
standardGeneric("branch_target")


.__C__git_fetch_head <- new("classRepresentation"
    , slots = structure(list(ref_name = structure("character", package = "methods"), 
    remote_url = structure("character", package = "methods"), 
    sha = structure("character", package = "methods"), is_merge = structure("logical", package = "methods"), 
    repo = structure("git_repository", package = "git2r")), .Names = c("ref_name", 
"remote_url", "sha", "is_merge", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_fetch_head", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__ahead_behind:git2r` <- "<environment>"

`.__T__branch_get_upstream:git2r` <- "<environment>"

is_detached <- function (repo) 
standardGeneric("is_detached")


tree <- function (object) 
standardGeneric("tree")


fetch <- function (repo, name, credentials = NULL, verbose = TRUE, refspec = NULL) 
standardGeneric("fetch")


.__C__git_repository <- new("classRepresentation"
    , slots = structure(list(path = structure("character", package = "methods")), .Names = "path")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- character()
    can_open <- .Call(git2r_repository_can_open, object@path)
    if (!identical(can_open, TRUE)) 
        errors <- c(errors, "Unable to open repository at 'path'")
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_repository", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


remote_remove <- function (repo, name) 
standardGeneric("remote_remove")


`.__T__branch_set_upstream:git2r` <- "<environment>"

`.__T__is_shallow:git2r` <- "<environment>"

contributions <- function (repo, breaks = c("month", "year", "quarter", "week", 
    "day"), by = c("commits", "author")) 
standardGeneric("contributions")


`.__T__remote_ls:git2r` <- "<environment>"

merge <- function (x, y, ...) 
standardGeneric("merge")


`.__T__reflog:git2r` <- "<environment>"

.__C__git_blame <- new("classRepresentation"
    , slots = structure(list(path = structure("character", package = "methods"), 
    hunks = structure("list", package = "methods"), repo = structure("git_repository", package = "git2r")), .Names = c("path", 
"hunks", "repo"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    errors <- character()
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("git_blame", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


config <- function (repo = NULL, global = FALSE, user.name, user.email, 
    ...) 
{
    if (is.null(repo)) {
        repo <- discover_repository(getwd())
        if (!is.null(repo)) 
            repo <- repository(repo)
    }
    stopifnot(any(identical(global, TRUE), identical(global, 
        FALSE)))
    variables <- list(...)
    if (!missing(user.name)) 
        variables <- c(variables, list(user.name = user.name))
    if (!missing(user.email)) 
        variables <- c(variables, list(user.email = user.email))
    if (length(variables)) {
        for (i in seq_len(length(variables))) {
            if (!is.null(variables[[i]])) {
                if (!is.character(variables[[i]])) {
                  stop(paste0("'", names(variables)[i], "' must be a character vector"))
                }
            }
        }
        if (identical(global, TRUE)) {
            repo <- NULL
        }
        else if (is.null(repo)) {
            stop("Unable to locate local repository")
        }
        .Call(git2r_config_set, repo, variables)
    }
    cfg <- .Call(git2r_config_get, repo)
    cfg <- structure(lapply(cfg, function(x) x[order(names(x))]), 
        class = "git_config")
    if (length(variables)) {
        invisible(cfg)
    }
    else {
        return(cfg)
    }
}


is_blob <- function (object) 
{
    is(object = object, class2 = "git_blob")
}


`.__T__tags:git2r` <- "<environment>"

stash_drop <- function (object, ...) 
standardGeneric("stash_drop")


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

.__C__git_diff_hunk <- new("classRepresentation"
    , slots = structure(list(old_start = structure("integer", package = "methods"), 
    old_lines = structure("integer", package = "methods"), new_start = structure("integer", package = "methods"), 
    new_lines = structure("integer", package = "methods"), header = structure("character", package = "methods"), 
    lines = structure("list", package = "methods")), .Names = c("old_start", 
"old_lines", "new_start", "new_lines", "header", "lines"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("git_diff_hunk", package = "git2r")
    , package = "git2r"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__is_merge:git2r` <- "<environment>"

`.__T__reset:git2r` <- "<environment>"

fetch_heads <- function (repo) 
standardGeneric("fetch_heads")


`.__T__is_bare:git2r` <- "<environment>"

`.__T__hashfile:git2r` <- "<environment>"

`.__T__clone:git2r` <- "<environment>"

`.__T__odb_objects:git2r` <- "<environment>"

`.__T__descendant_of:git2r` <- "<environment>"

checkout <- function (object, ...) 
standardGeneric("checkout")




## Package Data

# none


## Package Info

.skeleton_package_title = "Provides Access to Git Repositories"

.skeleton_package_version = "0.18.0"

.skeleton_package_depends = "methods"

.skeleton_package_imports = "graphics,utils"


## Internal

.skeleton_version = 5


## EOF