let
  plutus = import ../../. {};
  serverTemplate = import ./server.nix;
  prometheusTemplate = import ./prometheus.nix;
  machines = (plutus.pkgs.lib.importJSON ./machines.json);
  overlays = import ./overlays.nix;
  secrets = (plutus.pkgs.lib.importJSON ./secrets.json);
  enableGithubHooks = plutus.pkgs.lib.hasAttr "githubWebhookKey" secrets;
  deploymentConfigDir = plutus.pkgs.copyPathToStore ../nixops ;
  deploymentServer = plutus.haskell.packages.deployment-server.components.exes.deployment-server-exe;
  plutusUrl = "https://${machines.environment}.${machines.plutusTld}";
  marloweUrl = "https://${machines.environment}.${machines.marloweTld}";
  mkConfig = ghsecrets: redirectUrl: callbackUrl: name: plutus.pkgs.writeTextFile {
    name = name;
    text = ''
    auth:
      # Maintainers' notes:
      # 1) Github keys and URL in here *must* match the ones set up for this app on
      #    github.
      # 2) If you change the JWT signature, it will break all existing logins.
      #    Don't change it unless that's something you specifically want!
      github-client-id: ${ghsecrets.githubClientId}
      github-client-secret: ${ghsecrets.githubClientSecret}
      jwt-signature: ${ghsecrets.jwtSignature}
      redirect-url: ${redirectUrl}
    marlowe:
      # The API Gateway url to trigger the marlowe symbolic lambda
      symbolic-url: "${machines.marloweSymbolicUrl}"
      api-key: "${secrets.apiGatewayKey}"
      callback-url: "${callbackUrl}"
    '';
  };
  playgroundConfig = mkConfig secrets.plutus plutusUrl "" "playground.yaml";
  marlowePlaygroundConfig = callbackUrl: mkConfig secrets.marlowe marloweUrl callbackUrl "marlowe.yaml";
  stdOverlays = [ overlays.journalbeat ];
  nixpkgsLocation = https://github.com/NixOS/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz;
  nixosLocation = "/root/.nix-defexpr/channels/nixos";
  slackChannel = "plutus-notifications";
  nixopsStateFile = "/root/.nixops/deployments.nixops";
  deploymentName = "playgrounds";
  options = { inherit stdOverlays machines defaultMachine plutus secrets nixpkgsLocation nixosLocation slackChannel nixopsStateFile deploymentName; };
  defaultMachine = (import ./default-machine.nix) options;
  marlowePlaygroundOptions = callbackUrl: options // { serviceConfig = marlowePlaygroundConfig callbackUrl;
                                                       serviceName = "marlowe-playground";
                                                       server-invoker = plutus.marlowe-playground.server-invoker;
                                                       client = plutus.marlowe-playground.client;
                                                       };
  playgroundOptions = options // { serviceConfig = playgroundConfig;
                                   serviceName = "plutus-playground";
                                   server-invoker = plutus.plutus-playground.server-invoker;
                                   client = plutus.plutus-playground.client;
                                   };
  playgroundA = serverTemplate.mkInstance playgroundOptions machines.playgroundA;
  playgroundB = serverTemplate.mkInstance playgroundOptions machines.playgroundB;
  marlowePlaygroundA = serverTemplate.mkInstance (marlowePlaygroundOptions (marloweUrl + "/machine-a/api")) machines.marlowePlaygroundA;
  marlowePlaygroundB = serverTemplate.mkInstance (marlowePlaygroundOptions (marloweUrl + "/machine-b/api")) machines.marlowePlaygroundB;
  nixops = prometheusTemplate.mkInstance 
            (options // {configDir = deploymentConfigDir; inherit deploymentServer enableGithubHooks;}) 
            {dns = "nixops.internal.${machines.environment}.${machines.plutusTld}";
             ip = "127.0.0.1";
             name = "nixops"; };
in
  { inherit playgroundA playgroundB marlowePlaygroundA marlowePlaygroundB nixops; }
