# annals
EMACS task based session manager and developer notebook

When working in a task, such as a Jira issue, it would be nice to keep the state of EMACS, including non-file buffers like \*shell\*.  EMACS desktop.el already does this.  In addition, I would like to:
- Optionally save the contents of earmuff buffers (\*shell\*, \*grep\*, etc), keeping a version per session
- Manage a directory of desktops based on task name or JIRA issue id
- Easily switch tasks
- Allow multiple tasks to be active at the same time.  Buffers can belong to one or more active tasks.

Of course it would rely heavily upon the built in desktop functionality.  

## Use Cases

Start a new task (`annals-new-task`)

> The user gives the task a name and it is created in a subdirectory of `annals-active-directory`.  It becomes the current task and any new buffers will belong to it.  

Suspend a task (`annals-suspend-task`)

> Do the same as `desktop-save` and `desktop-clear`. In addition, save off the state of interesting non-file buffers.

Resume a task (`annals-resume-task`)

> Do the same as `desktop-read`.  Do not reload the non-file buffers.

Archive a task (`annal-archive-task`)

> Compress a task and move it to the `annals-archive-directory`.  If the task was active, suspend it first.


annals-checkpoint
