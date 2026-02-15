local M = {}

function M.winbar()
  local ok, oil = pcall(require, "oil")
  if not ok then
    return ""
  end

  local dir = oil.get_current_dir()
  if not dir then
    return ""
  end

  return vim.fn.fnamemodify(dir, ":~")
end

function M.setup()
  _G.oil_winbar = M.winbar
end

return M
