# Changelog
## 2.6.0 (2025-07-21)
- Add ability to bulk change issues status

## 2.5.0 (2025-07-21)
- Add ability to add/remove watchers on an issue

## 2.4.0 (2025-07-19)
- Add support for favorite JQL filters.

## 2.3.0 (2025-07-19)
- Support panel block formatting in issue detail view.

## 2.2.0 (2025-07-17)
- Allow the creation of subtasks from issue detail view.

## 2.1.0 (2025-07-08)
- Add new section for subtasks in the detail view.
- Prepend appropriate emojis to headings in the detail view.

## 2.0.5 (2025-07-07)
- Add `P` keybinding to jump to the detail view of the parent issue.
- Add "Show parent issue" to the transient menu.

## 2.0.4 (2025-07-04)
- Find an issue by key or URL from the issues list view by pressing 'f'.

## 2.0.3 (2025-06-30)
- Add `jira-detail-show-announcements` custom variable to control announcements in detail view.
- Add announcement in Jira detail view about new update field feature.

## 2.0.2 (2025-06-30)
- Fix: Labels can now be updated correctly, handling the API's expectation of a list of strings.
## 2.0.1 (2025-06-30)
- Fix bug while updating the status of an issue. The original estimation was updated
  instead of the remaining time
## 2.0.0 (2025-06-29)
- **Major Version Update**
- **Enhanced Issue Management from Detail View:**
  - Seamlessly change issue statuses with a quick press of `C`.
  - Update various issue fields effortlessly using `U`. Enjoy intelligent
    suggestions for many fields, pulled directly from the Jira API, making
	updates faster and more accurate.
  - Press `?` to reveal a comprehensive menu of all available actions and keybindings.

## 1.1.5 (2025-06-27)
- Press `g` in Jira Detail mode to refresh the issue

## 1.1.4 (2025-06-21)
- Provide an `Unresolved` resolution value for issues.  Jira UI represents with
  the "Unresolved" key the resolutions for fields that don't have a resolution
  set.

## 1.1.3 (2025-06-08)
- Fix another critical bug with formatting of issue fields that can't be empty

## 1.1.2 (2025-06-06)
- Fix critical bug with formatting of issue fields that can't be empty

## 1.1.1 (2025-06-04)
- Fix critical bug that wasn't allowing to open Jira Issue Detail view

## 1.1.0 (2025-06-03)
- Add keybinding to remove a comment from issue detail view
- Add help transient in Issue Detail mode, press '?' to open it

## 1.0.3 (2025-05-26)
- Fix another formatting issue in detail view

## 1.0.2 (2025-05-26)
- Fix formatting issues with progress fields

## 1.0.1 (2025-05-26)
- Fix formatting issues in `jira-tempo`

## 1.0.0 (2025-05-23)
- **Breaking Change**: `jira-statuses-done`, `jira-statuses-progress`,
`jira-statuses-todo`, `jira-statuses-error` doesn't work any more.

	- Replaced status lists by status categories to **group statuses** in
      `Todo`, `In progress` and `Done`, improving the maintainability of the
      config. Each category have an associated face.

    - Added possibility to custom faces based on status names, allowing to
      override status categories.

## 0.11.0 (2025-05-19)
- Display more kinds of markup in comments and descriptions: numeric labels for
  orderedLists, indentation for nested lists, user mentions, emoji, calendar
  dates, italic, underline, strike-through, inline code, super/subscript,
  foreground color, adapting to dark backgrounds


## 0.10.0 (2025-05-17):
- Write comments in a new buffer

## 0.9.1 (2025-04-17)
- Add logging messages to find problems with API auth

## 0.9.0 (2025-04-17)
- Display times in locale's preferred format
- Add defcustom to display comments in chronological order

## 0.8.0 (2025-04-09):
- Add support for attachments (thank you @danielcmccarthy!)

## 0.7.0 (2025-03-29):
- Add comments from jira detail view

## 0.6.0 (2025-03-28)
- Add keybinding in issue to jump to tempo and the other way around.
- Close the jira issues buffer before I start the jira-tempo one, to avoid issues
- Avoid deleting other windows when opening jira-issues

## 0.5.0 (2025-03-24)
- Add support for bold text and code in docs
- Show comments in issue detail view

## 0.4.0 (2025-03-23)
- Add `jira-issues-table-fields` so that users can select the fields
  to be visualized in the table
- `--current-sprint` is not set by default any more when retrieving
  issues

## 0.3.0 (2024-03-21)
- Add support for Jira Personal Access Token (PAT)

## 0.2.2 (2025-03-20)
- Convert windows line endings to unix ones in issues detail

## 0.2.1 (2025-03-20)
- Add constant with package version: `jira-version`

## 0.2.0 (2025-03-19)
- Add support for authentication with `auth-source`

## 0.1.1 (2025-03-18)
- Fix bug for missing fields in issue detail view

## 0.1.0 (2025-03-18)
- Allow configuring Jira REST API Version

## 0.0.2 (2025-03-17)
- Log requests input data (if `jira-debug`)
- Provide date suggestions while adding a worklog

## 0.0.1 (2025-03-16)
- First working version
