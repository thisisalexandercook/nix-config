local M = {}

function M.setup()
  -- Refresh buffers changed on disk on focus/idle events.
  -- checktime does not overwrite unsaved edits.
  vim.api.nvim_create_autocmd(
    { "FocusGained", "BufEnter", "CursorHold", "CursorHoldI", "TermClose", "TermLeave", "VimResume" },
    {
      pattern = "*",
      callback = function()
        vim.cmd("silent! checktime")
      end,
    }
  )

  vim.api.nvim_create_autocmd("FileChangedShellPost", {
    pattern = "*",
    callback = function()
      vim.notify("File changed on disk. Buffer reloaded.", vim.log.levels.INFO)
    end,
  })
end

return M
