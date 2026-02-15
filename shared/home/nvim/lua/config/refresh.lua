local M = {}

function M.run()
  if vim.bo.filetype == "fugitive" then
    vim.cmd("edit")
    return
  end

  vim.cmd("silent! checktime")
end

return M
