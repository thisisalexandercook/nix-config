local M = {}

local function apply_no_bg()
  local groups = {
    "GitSignsAdd",
    "GitSignsChange",
    "GitSignsDelete",
    "GitSignsTopdelete",
    "GitSignsChangedelete",
    "GitSignsUntracked",
    "GitSignsStagedAdd",
    "GitSignsStagedChange",
    "GitSignsStagedDelete",
    "GitSignsStagedTopdelete",
    "GitSignsStagedChangedelete",
    "GitSignsStagedUntracked",
  }

  for _, group in ipairs(groups) do
    vim.cmd(("highlight %s guibg=NONE ctermbg=NONE"):format(group))
  end
end

function M.setup()
  apply_no_bg()
  vim.api.nvim_create_autocmd("ColorScheme", {
    callback = apply_no_bg,
  })
end

return M
