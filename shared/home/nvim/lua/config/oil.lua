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

local function normalize_dir(path)
  if not path or path == "" then
    return nil
  end
  return vim.fn.fnamemodify(path, ":p")
end

local function pick_dir_from_entry(entry)
  if not entry then
    return nil
  end
  return entry.path or entry.value or entry[1]
end

function M.find_directory_with_telescope()
  local ok_builtin, builtin = pcall(require, "telescope.builtin")
  if not ok_builtin then
    vim.notify("Telescope is not available", vim.log.levels.ERROR)
    return
  end

  local ok_actions, actions = pcall(require, "telescope.actions")
  local ok_state, action_state = pcall(require, "telescope.actions.state")
  if not ok_actions or not ok_state then
    vim.notify("Telescope actions are not available", vim.log.levels.ERROR)
    return
  end

  local find_command
  if vim.fn.executable("fd") == 1 then
    find_command = { "fd", "--type", "directory", "--hidden", "--exclude", ".git" }
  else
    find_command = { "find", ".", "-type", "d", "-not", "-path", "*/.git/*" }
  end

  builtin.find_files({
    prompt_title = "Directories (open in Oil)",
    find_command = find_command,
    attach_mappings = function(prompt_bufnr)
      actions.select_default:replace(function()
        local entry = action_state.get_selected_entry()
        actions.close(prompt_bufnr)

        local dir = normalize_dir(pick_dir_from_entry(entry))
        if not dir then
          return
        end

        local ok_oil, oil = pcall(require, "oil")
        if not ok_oil then
          vim.notify("Oil is not available", vim.log.levels.ERROR)
          return
        end
        oil.open(dir)
      end)
      return true
    end,
  })
end

function M.copy_current_dir()
  local ok_oil, oil = pcall(require, "oil")
  if not ok_oil then
    vim.notify("Oil is not available", vim.log.levels.ERROR)
    return
  end

  local dir = normalize_dir(oil.get_current_dir())
  if not dir then
    local current_file = vim.api.nvim_buf_get_name(0)
    if current_file ~= "" then
      dir = normalize_dir(vim.fn.fnamemodify(current_file, ":h"))
    end
  end

  if not dir then
    vim.notify("No directory to copy", vim.log.levels.WARN)
    return
  end

  vim.fn.setreg("+", dir)
  vim.fn.setreg('"', dir)
  vim.notify("Copied directory: " .. vim.fn.fnamemodify(dir, ":~"), vim.log.levels.INFO)
end

return M
