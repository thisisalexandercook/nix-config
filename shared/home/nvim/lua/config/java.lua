local M = {}

function M.setup()
  local ok_jdtls, jdtls = pcall(require, "jdtls")
  if ok_jdtls and vim.env.JOL_CLI_JAR and vim.env.JOL_CLI_JAR ~= "" then
    jdtls.jol_path = vim.env.JOL_CLI_JAR
  end

  local ok_dap, dap = pcall(require, "dap")
  local ok_dapui, dapui = pcall(require, "dapui")
  if ok_dap and ok_dapui then
    dap.listeners.before.attach.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.launch.dapui_config = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated.dapui_config = function()
      dapui.close()
    end
    dap.listeners.before.event_exited.dapui_config = function()
      dapui.close()
    end
  end

  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
      local client_id = args.data and args.data.client_id
      if not client_id then
        return
      end
      local client = vim.lsp.get_client_by_id(client_id)
      if not client or client.name ~= "jdtls" then
        return
      end
      if vim.bo[args.buf].filetype ~= "java" then
        return
      end
      if vim.b[args.buf].jdtls_dap_ready then
        return
      end

      if not ok_jdtls then
        return
      end

      pcall(jdtls.setup_dap, { hotcodereplace = "auto" })
      pcall(function()
        require("jdtls.dap").setup_dap_main_class_configs()
      end)
      vim.b[args.buf].jdtls_dap_ready = true
    end,
  })
end

return M
