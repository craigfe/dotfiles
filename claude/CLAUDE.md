# GitHub PR Comment Integration

## Responding to pull request comments

You can use the GitHub CLI to fetch PR comments when responding to a code review

```bash
gh pr view PR_NUMBER --repo OWNER/REPO --json comments --jq '.comments[] | {author: .author.login, createdAt: .createdAt, body: .body}'
```
