# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs Lisp package that provides an interface to the Jira CLI tool (`jira-cli` by ankitpokhrel). It's a single-file package (`jira.el`) that wraps the Jira CLI and provides an interactive Emacs interface for viewing and managing Jira issues, with enhanced Emacspeak support for accessibility.

## Architecture

### Core Components

- **Command Execution Layer** (`jira--run-command`, `jira--run-command-json`): Wraps calls to the external `jira` CLI tool, handling project defaults and stripping ANSI color codes from output
- **Issue Parsing** (`jira--parse-issue-list-output`): Parses tab-separated CLI output into Emacs property lists
- **Display Layer** (`jira--display-issues`, `jira--insert-issue`): Renders issues in a tabular format with text properties for navigation
- **Text Properties**: Each issue line stores `:jira-issue-key` and `:jira-issue-data` as text properties for point-based operations
- **Emacspeak Integration**: Custom line reading (`jira--emacspeak-speak-line`) and auditory icons for accessibility

### Data Flow

1. User invokes command (e.g., `jira-list-issues`)
2. Command runs `jira` CLI with appropriate arguments
3. ANSI codes stripped from output
4. Tab-separated output parsed into property lists
5. Issues inserted into buffer with text properties
6. Text properties enable all point-based operations (view, edit, assign, etc.)

### Key Design Patterns

- Uses text properties (`jira-issue-key`, `jira-issue-data`, `emacspeak-speak`) to attach metadata to display lines
- Buffer-local variables (`jira-current-project`, `jira-current-filters`) track current view state
- All issue operations use `jira--parse-issue-key-at-point` to get the current issue
- Refresh preserves line position after reloading data

## Development Commands

Since this is an Emacs Lisp package without a build system:

### Testing in Emacs

```elisp
;; Load the package
(load-file "jira.el")

;; Or with use-package
(use-package jira
  :ensure nil
  :load-path "~/projects/robertmeta/jira-wrap")
```

### Manual Testing

1. Ensure jira-cli is installed and configured: `jira init`
2. Load `jira.el` in Emacs
3. Run `M-x jira-list-issues`

### Testing jira-cli Output

When debugging parsing issues, test the CLI directly:

```bash
# Test plain output format (what jira.el parses)
jira issue list --plain --no-headers

# Test filtered output (for assignee-specific views)
jira issue list -a $(jira me) --plain --no-headers
```

## Known Constraints

- **External Dependency**: Requires `jira-cli` to be installed and configured
- **Output Parsing**: Parsing logic in `jira--parse-issue-list-output` depends on tab-separated output format from jira-cli
- **ANSI Stripping**: Must strip ANSI codes since jira-cli outputs colored text by default
- **Interactive Creation**: Issue creation uses jira-cli's interactive mode rather than pure Emacs forms
- **Single File Package**: All code in one file (`jira.el`) - no module system
- **Assignee Limitation**: When listing all issues (unfiltered), assignee information is not available in the plain output. The assignee field will show "Unassigned" for all issues. Use filtered views (`M` for my issues, `A` for assignee filter) to see accurate assignee data.

## Interactive Commands

Beyond `jira-list-issues`, these standalone commands are also available:

- `jira-my-issues` - Directly open issues assigned to you
- `jira-watching` - Show issues you're watching
- `jira-view-issue` - View a specific issue by key

## Code Conventions

- Use `jira--` prefix for private functions
- Use `jira-` prefix for interactive commands and customization variables
- Buffer-local variables use `defvar-local`
- Emacspeak integration uses `with-eval-after-load` and advice to avoid hard dependency
- Evil mode integration also uses `with-eval-after-load` to avoid hard dependency
