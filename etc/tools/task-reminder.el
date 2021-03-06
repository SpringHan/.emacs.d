;;;; This is the task-reminder tool for Spring Emacs

(defgroup task-reminder nil
  "The group for the task reminder's variables."
  :group 'applications)

(defcustom task-reminder-status nil
  "The task-reminder program's status."
  :type 'symbol
  :group 'task-reminder)

(defcustom task-reminder-tasks nil
  "The tasks of task-reminder."
  :type 'list
  :group 'task-reminder)

(defcustom task-reminder-timer nil
  "The timer for task-reminder."
  :group 'task-reminder)

(defconst task-reminder-file (expand-file-name (locate-user-emacs-file "tasks-list"))
  "The task-reminder's local file for saving the tasks.")

(defun task-reminder (key)
  "The main function about task reminder."
  (interactive (list (completing-read "Enter the function: "
                                      '("add" "remove" "stop" "start"))))
  (pcase key
    ("add" (task-reminder-add-task))
    ("start" (task-reminder-start))))

(defun task-reminder-add-task (task-name task-time task-content)
  "Add task."
  (interactive (list (read-string "Enter the task name: ")
                     (read-string "Enter the deadline time(HH:mm): ")
                     (read-string "Enter the task content: "))))

(defun task-reminder-get-tasks ()
  "The function to get the tasks from task file.")

(defun task-reminder-get-task-status (task)
  "Get the TASK's status and return it.
When it's done, return t.
Otherwise return nil.")

(defun task-reminder-process ()
  "Start the process."
  (when (null task-reminder-status)
    (cancel-timer task-reminder-timer)
    (setq task-reminder-timer nil)))

(defun task-reminder-start ()
  "The task-reminder main program."
  (interactive)
  (if (not (null task-reminder-status))
      (message "[Spring Emacs]: Task-Reminder has already running!")
    (setq task-reminder-timer
          (run-at-time t 5 'task-reminder-process))))

(provide 'task-reminder)
