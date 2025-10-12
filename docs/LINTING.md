# R Linting Configuration

This repository is configured with automated R code linting using the `lintr` package.

## Configuration Files

### `.lintr`
The main linting configuration file with project-specific rules:
- Line length limit: 120 characters
- Disabled linters: object naming, trailing blank lines, commented code, T/F symbols
- Cyclomatic complexity threshold: 25
- Excluded directories: `renv/`, `man/`, `tmp/`, `.Rproj.user/`

### `.vscode/settings.json`
VS Code integration for real-time linting feedback in the editor:
- Enables R Language Server diagnostics
- Activates lintr integration
- Configures file associations

## Usage

### Command Line

Run linting on the entire package:
```bash
make lint
```

Run code styling (auto-format):
```bash
make style
```

### In VS Code

Linting runs automatically when you open R files. Issues are highlighted with:
- **Warnings** (yellow squiggles)
- **Errors** (red squiggles)
- **Style suggestions** (blue squiggles)

## Common Issues Found

Based on the initial lint run, the main issues to address are:

1. **Indentation inconsistencies** - Most files need standardized indentation
2. **Trailing whitespace** - Clean up spaces at end of lines
3. **Line length** - Some lines exceed 120 characters
4. **Syntax errors** - Critical issues like misplaced parentheses (see R/align.R line 214-221)
5. **Assignment operators** - Use `<-` instead of `=` for assignments
6. **Object usage warnings** - Missing imports or undefined functions

## Priority Fixes

### Critical (Syntax Errors)
- **R/align.R:221** - Extra closing parenthesis in H1 function

### High Priority (Style)
- Fix indentation across all files
- Remove trailing whitespace
- Break long lines (>120 chars)

### Medium Priority
- Add missing package imports to NAMESPACE
- Document global variable usage with roxygen tags

## Customizing Rules

Edit `.lintr` to modify linting rules. Common customizations:

```r
# Change line length limit
line_length_linter(100)

# Enable object naming checks
object_name_linter("snake_case")

# Adjust complexity threshold
cyclocomp_linter(15)
```

## CI/CD Integration

The `make lint` command can be added to your CI/CD pipeline to enforce code quality:

```yaml
# Example GitHub Actions
- name: Lint R code
  run: make lint
```

## Additional Tools

- **styler**: Auto-format code with `make style`
- **goodpractice**: Run comprehensive package checks
- **covr**: Check test coverage

## Resources

- [lintr Documentation](https://lintr.r-lib.org/)
- [styler Documentation](https://styler.r-lib.org/)
- [R Package Development Guide](https://r-pkgs.org/)
