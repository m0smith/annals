# annals
EMACS task based session manager and developer notebook

When working in a task, such as a Jira issue, it would be nice to keep the state of EMACS, including non-file buffers like \*shell\*.  EMACS desktop.el already does this.  In addition, I would like to:
- Optionally save the contents of earmuff buffers (\*shell\*, \*grep\*, etc), keeping a version per session ([gh-1](https://github.com/m0smith/annals/issues/1))
- Manage a directory of desktops based on task name or JIRA issue id or github issue id (`annals-task`)
- Integerate with org-mode task switching, clock in, clock out, etc.
- Integerate with dir-mode by adding keystrokes to use a dir as a desktop ([gh-3](https://github.com/m0smith/annals/issues/3))
- Easily switch tasks (`annals-task`)
- Allow multiple tasks to be active at the same time.  Buffers can belong to one or more active tasks.

Of course it would rely heavily upon the built in desktop functionality.  

## Use Cases

Start a new task (`annals-task`)

> The user gives the task a name and it is created in a subdirectory of `annals-active-directory`.  It becomes the current task and any new buffers will belong to it.  Creates an annals.org in the task directory.  If the task id matches a Jira issue or github issue, pull information from that issue into the annals.org.

Suspend a task (`annals-suspend`)

> Do the same as `desktop-save` and `desktop-clear`. In addition, save off the state of interesting non-file buffers.

Resume a task (`annals-task`)

> Do the same as `desktop-read`.  Do not reload the non-file buffers.

Archive a task (`annals-archive`)

> Compress a task and move it to the `annals-archive-directory`.  If the task was active, suspend it first.

Save the state of active tasks and keep them all active (`annals-checkpoint`)

> Same as `desktop-save` on all the tasks.

## Hooks

`annals-task-hook` Run after the task has been initialized or switched.  The current task id will be in the variable `annals-active-task-id`.
