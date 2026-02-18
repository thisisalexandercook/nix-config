local M = {}

function M.run()
  if vim.bo.filetype == "fugitive" then
    vim.cmd("edit")
    return
  end

  if vim.bo.modified then
    vim.notify("Buffer has unsaved changes; skipping refresh.", vim.log.levels.WARN)
    return
  end

  if vim.bo.buftype == "" and vim.api.nvim_buf_get_name(0) ~= "" then
    -- For file-backed buffers, force a clean reread from disk.
    vim.cmd("silent! edit")
    return
  end

  vim.cmd("silent! checktime")
end

return M
