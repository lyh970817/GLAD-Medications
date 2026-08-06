# Zapier MCP setup for this repo

This repository is now wired so Codex can use Zapier MCP when `ZAPIER_MCP_BEARER_TOKEN` is present in your local environment.

## 1) Verify MCP server registration in Codex

Already configured globally as:

- Name: `zapier`
- URL: `https://mcp.zapier.com/api/mcp/mcp`
- Auth env var: `ZAPIER_MCP_BEARER_TOKEN`

Verify with:

```bash
codex mcp get zapier --json
```

## 2) Add your local bearer token (not committed)

Create a local env file from template:

```bash
cp .envrc.local.example .envrc.local
```

Edit `.envrc.local` and set your real token:

```bash
export ZAPIER_MCP_BEARER_TOKEN="..."
```

Then allow direnv to load it:

```bash
direnv allow
```

`./.envrc` already includes:

```bash
source_env_if_exists .envrc.local
```

So the token is loaded only on your machine.

## 3) Quick connectivity check

Inside this repo shell:

```bash
echo ${ZAPIER_MCP_BEARER_TOKEN:+set}
codex mcp list
```

If token is set, Codex sessions started from this repo can call Zapier MCP tools.

## 4) Optional: re-add configuration manually

```bash
codex mcp remove zapier
codex mcp add zapier --url https://mcp.zapier.com/api/mcp/mcp --bearer-token-env-var ZAPIER_MCP_BEARER_TOKEN
```

