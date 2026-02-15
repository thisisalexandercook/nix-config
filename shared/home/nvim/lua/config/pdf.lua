local M = {}

local function open_in_zathura(file, src_buf)
  vim.fn.jobstart({ "zathura", file }, { detach = true })
  vim.schedule(function()
    if vim.api.nvim_buf_is_valid(src_buf) then
      vim.api.nvim_buf_delete(src_buf, { force = true })
    end
  end)
end

local function open_in_tdf(file, src_buf)
  if vim.env.TMUX == nil or vim.env.TMUX == "" then
    vim.notify("tdf requires tmux for pane split. Open inside tmux or choose zathura.", vim.log.levels.WARN)
    return
  end

  local cwd = vim.fn.fnamemodify(file, ":h")
  local cmd = "exec tdf " .. vim.fn.shellescape(file)
  vim.fn.jobstart({ "tmux", "split-window", "-h", "-c", cwd, cmd }, { detach = true })

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

  local choices = {
    { label = "zathura (GUI)", value = "zathura" },
    { label = "tdf (terminal)", value = "tdf" },
  }

  vim.ui.select(choices, {
    prompt = "Open PDF with:",
    format_item = function(item)
      return item.label
    end,
  }, function(choice)
    if not choice then
      return
    end

    if choice.value == "tdf" then
      open_in_tdf(file, src_buf)
      return
    end

    open_in_zathura(file, src_buf)
  end)
end

return M
