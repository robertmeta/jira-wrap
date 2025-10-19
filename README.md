# jira.el - Emacs Interface for Jira CLI

An Emacs package providing a full-featured interface to the Jira CLI tool, with enhanced accessibility support for Emacspeak users.

## Features

- **View and manage Jira issues** in an interactive list
- **Create, edit, and delete** issues
- **Assign issues** to team members
- **Move issues** through workflow states
- **Add comments** to issues
- **Filter issues** by status, type, assignee, priority
- **View sprints** and sprint issues
- **My issues** and **watching** quick filters
- **Open in browser** for detailed web view
- **Enhanced Emacspeak support** with custom line reading and auditory icons
- **Evil mode integration** (starts in Emacs state)

## Requirements

- Emacs 27.1 or later
- [Jira CLI](https://github.com/ankitpokhrel/jira-cli) installed and configured
- Optional: Emacspeak for enhanced audio feedback

## Installation

### Install and Configure Jira CLI

```bash
# Install jira-cli
brew install ankitpokhrel/jira-cli/jira-cli

# Initialize configuration
jira init
```

Follow the prompts to configure your Jira instance, credentials, and default project.

### Install the Emacs package

#### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/robertmeta/jira-wrap.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (use-package jira
     :ensure nil
     :load-path "~/path/to/jira-wrap"
     :commands (jira-list-issues)
     :init
     (setq jira-default-project "MYPROJECT")  ; Optional: override config default
     :bind (("s-J" . jira-list-issues)))
   ```

## Usage

### Commands

- `M-x jira-list-issues` - Open the Jira issues list (or `s-J` if bound)

### Keybindings in jira-mode

| Key   | Command                          |
|-------|----------------------------------|
| `RET` | View issue details               |
| `c`   | Create new issue                 |
| `e`   | Edit issue                       |
| `a`   | Assign issue                     |
| `m`   | Move issue to different status   |
| `C`   | Add comment                      |
| `g`   | Refresh view                     |
| `o`   | Open issue in browser            |
| `n/p` | Next/previous line               |
| `q`   | Quit window                      |

#### Filters

| Key | Command                |
|-----|------------------------|
| `s` | Filter by status       |
| `t` | Filter by type         |
| `A` | Filter by assignee     |
| `P` | Filter by priority     |
| `M` | Show my issues         |
| `W` | Show watching issues   |
| `S` | Show sprints           |

### Display Format

Issues are displayed in a table format:

```
KEY          TYPE       STATUS       ASSIGNEE        SUMMARY
--------------------------------------------------------------------------------
PROJ-123     Story      In Progress  john.doe        Add user authentication
PROJ-124     Bug        To Do        jane.smith      Fix login redirect
PROJ-125     Task       Done         Unassigned      Update documentation
```

## Emacspeak Support

This package includes comprehensive Emacspeak integration:

### Custom Line Reading

When navigating with Emacspeak, lines are read in a cleaner format:
- **Visual**: `PROJ-123     Story      In Progress  john.doe        Add user authentication`
- **Spoken**: "PROJ-123, Add user authentication, Story, assigned to john.doe, In Progress, high priority"

The spoken format:
- Starts with the issue key
- Includes the summary (most important)
- States the issue type
- Mentions the assignee
- Announces the status
- Includes priority if set
- Skips visual formatting noise

### Auditory Icons

- **Opening issue list**: "open-object" sound
- **Creating issue**: "item" sound
- **Moving issue**: "select-object" sound

### Voice Personalities

Future enhancement: Different voice personalities for different issue types or priorities.

## Configuration

### Customization Variables

```elisp
;; Default project (overrides jira config)
(setq jira-default-project "MYPROJECT")

;; Path to jira command (if not in PATH)
(setq jira-command "/opt/homebrew/bin/jira")

;; Number of issues to fetch at a time
(setq jira-issue-pagesize 50)
```

### Example Configuration

```elisp
(use-package jira
  :ensure nil
  :load-path "~/projects/robertmeta/jira-wrap"
  :commands (jira-list-issues jira-my-issues jira-watching)
  :init
  (setq jira-default-project "ACME")
  (setq jira-issue-pagesize 100)
  :bind (("s-J" . jira-list-issues)
         ("s-M-j" . jira-my-issues)))
```

## Workflow Examples

### View your assigned issues

1. `M-x jira-list-issues` - Open issue list
2. Press `M` - Filter to "my issues"
3. Press `RET` on an issue - View full details
4. Press `m` - Move to different status
5. Press `C` - Add a comment

### Create and assign a new issue

1. `M-x jira-list-issues` - Open issue list
2. Press `c` - Create new issue (opens interactive prompt)
3. Fill in details in the jira CLI interactive form
4. Press `a` - Assign the newly created issue
5. Enter assignee email or name

### Quick triage

1. `M-x jira-list-issues`
2. Press `s` - Filter by status (e.g., "To Do")
3. Navigate with `n`/`p`
4. Press `a` to assign
5. Press `m` to move through workflow
6. Press `g` to refresh and see changes

## Development

### Project Structure

```
jira-wrap/
├── README.md           # This file
├── jira.el            # Main package file
└── .gitignore         # Git ignore patterns
```

### Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

### Known Limitations

- Issue creation uses the Jira CLI's interactive mode (not yet a pure Emacs form)
- Advanced JQL queries not yet supported through the UI (can use `--jql` filter manually)
- Worklog management not yet implemented

### Roadmap

- [ ] Pure Emacs forms for issue creation/editing
- [ ] Advanced JQL query builder
- [ ] Worklog management
- [ ] Epic and sprint management
- [ ] Inline issue editing (without CLI interactive mode)
- [ ] Org-mode integration for issue tracking

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- Built for the [Jira CLI](https://github.com/ankitpokhrel/jira-cli) by Ankit Pokhrel
- Emacspeak integration inspired by T.V. Raman's Emacspeak project
- Created as an accessibility-first interface to Jira

## See Also

- [Jira CLI](https://github.com/ankitpokhrel/jira-cli) - The underlying CLI tool
- [Emacspeak](https://github.com/tvraman/emacspeak) - The complete audio desktop
- [reminders-wrap](https://github.com/robertmeta/reminders-wrap) - Similar package for macOS Reminders
