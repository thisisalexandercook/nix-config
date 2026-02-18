local M = {}

local function open_in_default_gui_viewer(file, src_buf)
  vim.fn.jobstart({ "xdg-open", file }, { detach = true })
  vim.schedule(function()
    if vim.api.nvim_buf_is_valid(src_buf) then
      vim.api.nvim_buf_delete(src_buf, { force = true })
    end
  end)
end

function M.open_current_pdf()
  local src_buf = vim.api.nvim_get_current_buf()
  local file = vim.api.nvim_buf_get_name(src_buf)
  if file == "" then
    return
  end

  open_in_default_gui_viewer(file, src_buf)
end

return M
