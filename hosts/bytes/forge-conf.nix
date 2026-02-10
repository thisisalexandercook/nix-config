{ stateDir, port, domain, rootUrl, extraServerSettings ? {} }:
{
  enable = true;
  database.type = "sqlite3";
  stateDir = stateDir;
  lfs.enable = true;

  settings = {
    server = {
      DOMAIN = domain;
      HTTP_ADDR = "0.0.0.0";
      HTTP_PORT = port;
      ROOT_URL = rootUrl;
    } // extraServerSettings;
    service.DISABLE_REGISTRATION = true;
  };
}
