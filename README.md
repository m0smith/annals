# annals
EMACS task based session manager and developer notebook

When working in a task, such as a Jira issue, it would be nice to keep the state of EMACS, including non-file buffers like \*shell\*.  EMACS desktop.el already does this.  In addition, I would like to:
- Optionally save the contents of earmuff buffers (\*shell\*, \*grep\*, etc), keeping a version per session ([gh-1](https://github.com/m0smith/annals/issues/1)) (`annals-buffer-name-create`)
- Manage a directory of desktops based on task name or JIRA issue id or github issue id (`annals-task`)
- Integerate with org-mode task switching, clock in, clock out, etc.
- Integerate with dir-mode by adding keystrokes to use a dir as a desktop ([gh-3](https://github.com/m0smith/annals/issues/3))
- Create a jounal file for each task called `annals.org`.  The name is controlled by `annals-file-name-default`.
- Easily switch tasks (`annals-task`)
- Allow multiple tasks to be active at the same time.  Buffers can belong to one or more active tasks.

Of course it would rely heavily upon the built in desktop functionality.  

## Installation and Configuration

Clone the repo from https://github.com/m0smith/annals.  The following instructions assume it is in `~/projects`.
Add to your `.emacs`:

``` emacs-lisp
(add-to-list 'load-path "~/projects/annals")
(load "annals")

(global-set-key [f6] (quote annals-task))
(global-set-key [C-f6] (quote annals-buffer-name-create))
```

## Basic Tasks

Start a new task (`annals-task`)

> The user gives the task a name and it is created in a subdirectory of `annals-active-directory`.  It becomes the current task and any new buffers will belong to it.  Creates an annals.org in the task directory.  If the task id matches a Jira issue or github issue, pull information from that issue into the annals.org.

Suspend a task (`annals-suspend`)

> Do the same as `desktop-save` and `desktop-clear`. In addition, save off the state of interesting non-file buffers.

Resume a task (`annals-task`)

> Do the same as `desktop-read`.  Do not reload the non-file buffers.

Archive a task (`annals-archive`)

> Move the task to the `annals-archive-directory`.  If the task was active, suspend it first.

Save the state of active tasks and keep them all active (`annals-checkpoint`)

> Same as `desktop-save` on all the active tasks.  In addition, save the associated non-file buffers.

Associate a non-file buffer to be saved as part of the annal (`annals-buffer-name-create`)

## Dired Mode Integration

A minor mode exists for for the active directory and the archive directory to manage the tasks.

When in dired-mode for the `annals-active-directory` (default ~/annals), a new minor mode that adds some annals functionality with the following bindings:

  - `|a` - `annals-dired-task`     Activate the task on the current line
  - `|z` - `annals-dired-archive`  Archive the task on the current line
  - `|i` - `annals-dired-info`     Display the summary info for the current task
  
When in dired-mode for the `annals-archive-directory` (default ~/annals/.archive), a new minor mode that adds some annals functionality with the following bindings:

  - `|z` - `annals-dired-unarchive`  Unarchive the task on the current line
  - `|i` - `annals-dired-info`     Display the summary info for the current task  

## Hooks

`annals-task-hook` Run after the task has been initialized or switched.  The current task id will be in the variable `annals-active-task-id`.

`annals-create-file-hook` Run when the annals.org file needs to be created for a task.   The function should accept 2 parameters: task-id and file-name and return the file-name or nil if it did not create the file.  The file file-name will not exist when the function is called. The functions in the list will be called until one returns non-nil, meaning it actually created a file.
