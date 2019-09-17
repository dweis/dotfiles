{ hiDpi, mainDisplay }:
{ config, pkgs, lib, ... }:

let
  name = "Derrick Weis";
  email = "derrick@derrickweis.com";
  githubUsername = "dweis";
  color = {
    # Dracula - https://github.com/dracula/dracula-theme
    background = "#282a36";
    currentLine = "#44475a";
    selection = "#44475a";
    foreground = "#f8f8f2";
    comment = "#6272a4";
    cyan = "#8be9fd";
    green = "#50fa7b";
    orange = "#ffb86c";
    pink = "#ff79c6";
    purple = "#bd93f9";
    red = "#ff5555";
    yellow = "#f1fa8c";
    # extra
    darkGray = "#040404";
    white = "#bfbfbf";
    black = "#000000";
    blue = "#caa9fa";
    brightRed = "#ff6e67";
    brightGreen = "#5af78e";
    brightYellow = "#f4f99d";
    brightBlue = "#caa9fa";
    brightMagenta = "#ff92d0";
    brightCyan = "#9aedfe";
    brightWhite = "#e6e6e6";
  };
  fontSize = if hiDpi then 14 else 10;
  monospaceFont = "Hack";
in {
  fonts.fontconfig.enable = true;

  home = {
    packages = with pkgs; [
      noto-fonts
      font-awesome-ttf
      material-icons
    ];

    file.".stack/config.yaml".text = ''
      templates:
        params:
          author-name: ${name}
          author-email: ${email}
          github-username: ${githubUsername}
    '';

    file.".ghci".text = ''
      :set editor ~/.nix-profile/bin/vi
    '';

    file.".haskeline".text = ''
      editMode: Vi
    '';

    file.".inputrc".text = ''
      set editing-mode vi
      set keymap vi
    '';

    file.".config/rofi/dracula.rasi".text = ''
      // Dracula colors
      * {
          background: ${color.background};
          current-line: ${color.currentLine};
          selection: ${color.selection};
          foreground: ${color.foreground};
          comment: ${color.comment};
          cyan: ${color.cyan};
          green: ${color.green};
          orange: ${color.orange};
          pink: ${color.pink};
          purple: ${color.purple};
          red: ${color.red};
          yellow: ${color.yellow};
      }
      * {
          selected-normal-background:     @cyan;
          normal-background:              @background;
          normal-foreground:              @foreground;
          alternate-normal-background:    @normal-background;
          alternate-normal-foreground:    @foreground;
          selected-normal-foreground:     @normal-background;
          urgent-foreground:              @red;
          urgent-background:              @normal-background;
          alternate-urgent-background:    @urgent-background;
          alternate-urgent-foreground:    @urgent-foreground;
          selected-active-foreground:     @foreground;
          selected-urgent-background:     @normal-background;
          alternate-active-background:    @normal-background;
          alternate-active-foreground:    @selected-active-foreground;
          alternate-active-background:    @selected-active-background;
          border-color:                   @selected-normal-background;
          separatorcolor:                 @border-color;
          spacing: 2;
          background-color: @normal-background;
      }
      #window {
          border:           3;
          padding:          9;
      }
      #mainbox {
          background-color: inherit;
          border:  0;
          padding: 5;
      }
      #textbox {
          text-color: @foreground;
      }
      #element {
          border:  0;
          padding: 1px ;
      }
      #element.normal.normal {
          background-color: @normal-background;
          text-color:       @normal-foreground;
      }
      #element.normal.urgent {
          background-color: @urgent-background;
          text-color:       @urgent-foreground;
      }
      #element.normal.active {
          background-color: @active-background;
          text-color:       @active-foreground;
      }
      #element.selected.normal {
          background-color: @selected-normal-background;
          text-color:       @selected-normal-foreground;
      }
      #element.selected.urgent {
          background-color: @selected-urgent-background;
          text-color:       @selected-urgent-foreground;
      }
      #element.selected.active {
          background-color: @selected-active-background;
          text-color:       @selected-active-foreground;
      }
      #element.alternate.normal {
          background-color: @alternate-normal-background;
          text-color:       @alternate-normal-foreground;
      }
      #element.alternate.urgent {
          background-color: @alternate-urgent-background;
          text-color:       @alternate-urgent-foreground;
      }
      #element.alternate.active {
          background-color: @alternate-active-background;
          text-color:       @alternate-active-foreground;
      }
      #scrollbar {
          border:       0;
      }
      #button.selected {
          background-color: @selected-normal-background;
          text-color:       @selected-normal-foreground;
      }
      #inputbar {
          spacing:    10;
          text-color: @normal-foreground;
          background-color: inherit;
          padding:    1px ;
      }
      #case-indicator {
          spacing:    0;
          text-color: @normal-background;
      }
      #entry {
          spacing:    0;
          text-color: @normal-foreground;
      }
      #prompt {
          spacing:    0;
          text-color: @normal-foreground;
      }
    '';
  };

  programs = {
    home-manager.enable = true;

    alacritty = {
      enable = true;
      settings = {
        window = {
          dimensions = {
            columns = 80;
            lines = 24;
          };
        };
        font = {
          normal = {
            family = monospaceFont;
          };
          bold = {
            family = monospaceFont;
          };
          italic = {
            family = monospaceFont;
          };
          size = fontSize;
        };
        draw_bold_text_with_bright_colors = true;
        colors = {
          primary = {
            background = "${color.background}";
            foreground = "${color.foreground}";
          };
          normal = {
            black =   "${color.black}";
            red =     "${color.red}";
            green =   "${color.green}";
            yellow =  "${color.yellow}";
            blue =    "${color.blue}";
            magenta = "${color.purple}";
            cyan =    "${color.cyan}";
            white =   "${color.white}";
          };
          bright = {
            black =   "${color.background}";
            red =     "${color.brightRed}";
            green =   "${color.brightGreen}";
            yellow =  "${color.brightYellow}";
            blue =    "${color.brightBlue}";
            magenta = "${color.brightMagenta}";
            cyan =    "${color.brightCyan}";
            white =   "${color.brightWhite}";
          };
        };
      };
    };

    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
    };

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      extraConfig =
        ''
        set number
        set nowrap
        set showmode
        set smartcase
        set smartindent
        set softtabstop=2
        set shiftwidth=2
        set expandtab
        set history=1000
        set colorcolumn=80,100
        set list listchars=trail:.

        set completeopt=menuone,menu,longest

        set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox,output,node_modules,bower_components
        set wildmode=longest,list,full

        set t_Co=256

        set cmdheight=1

        colorscheme gruvbox

        " -- Supertab
        let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

        if has("gui_running")
          imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
        else " no gui
          if has("unix")
            inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
          endif
        endif

        " -- NERDtree
        map <Leader>n :NERDTreeToggle<CR>

        " -- Ctrl-P
        map <silent> <Leader>t :CtrlP()<CR>
        noremap <leader>b<space> :CtrlPBuffer<cr>
        let g:ctrlp_custom_ignore = '\v[\/]dist$'

        " -- Airline
        let g:airline_powerline_fonts = 1
        '';

        plugins = with pkgs.vimPlugins; [
          vim-plug
          sensible
          airline
          vim-nix
          syntastic
          ctrlp
          deoplete-nvim
          editorconfig-vim
          gruvbox
          nerdcommenter
          nerdtree
          supertab
          vim-scala
          vim-plug
        ];
    };

    rofi = {
      enable = true;
      terminal = "\${pkgs.alacritty}/bin/alacritty";
      theme = "dracula";
      font = "${monospaceFont} 16";
      extraConfig = ''
        rofi.modi: combi
        rofi.drun-icon-theme: Numix Square
        rofi.show-icons: true
      '';
    };

    tmux = {
      enable = true;
      keyMode = "vi";
      terminal = "tmux-256color";
    };

    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        theme = "norm";
        plugins = [ "aws" ];
      };
      envExtra = ''
        EDITOR="vim"
        TERM="xterm-256color"
      '';
      shellAliases = {
        fonts = "fc-list | cut -f2 -d: | sort -u";
        nix-search = "nix-env -qaP '*' | grep";
        nix-cleanup = "nix-collect-garbage -d && nix-store --optimize";
      };
    };
  };

  services.compton = {
    enable = true;
    backend = "glx";
    blur = true;
    shadow = true;
    extraOptions = ''
      paint-on-overlay = true;
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
    '';
  };

  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3GapsSupport = true;
      alsaSupport = true;
      githubSupport = true;
      pulseSupport = true;
      jsoncpp = pkgs.jsoncpp;
    };
    config = {
      "settings" = {
        throttle-ms = 50;
        throttle-limit = 5;
      };
      "bar/main" = {
        monitor = "\${env:MONITOR:${mainDisplay}}";
        width = "100%";
        height = if hiDpi then "3%" else "1.5%";
        offset-y = 5;
        bottom = true;

        background = color.background;
        foreground = color.foreground;

        overline-size = 2;
        overline-color = color.cyan;
        underline-size = 2;
        underline-color = color.cyan;

        spacing = 1;
        padding-right = 2;
        module-margin-left = 0;
        module-margin-right = 2;

        font-0 = "Noto Sans:size=${toString fontSize};0";
        font-1 = "Material Icons:size=${toString (fontSize * 1.5)};4";
        font-2 = "FontAwesome:size=${toString (fontSize * 1.5)};4";

        modules-left = "i3";
        modules-center = "xwindow";
        modules-right = "cpu memory clock pulseaudio backlight battery";

        tray-position = "right";
        tray-maxsize = if hiDpi then 28 else 16;
        tray-scale = "1.0";
      };
      "module/backlight" = {
        type = "internal/backlight";
        format = "<ramp> <label>";
        card = "intel_backlight";
        ramp-0 = "🌔";
        ramp-1 = "🌔";
        ramp-2 = "🌓";
        ramp-3 = "🌒";
        ramp-4 = "🌑";
        bar-width = 10;
        bar-indicator = "|";
        bar-fill = "─";
        bar-empty = "─";
      };
      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%d.%m.%y";
        time = "%H:%M";
        label = "%time%  %date%";
      };
      "module/i3" = {
        type = "internal/i3";
        format = "<label-state> <label-mode>";
        format-spacing = 0;
        label-focused = "%index%";
        label-focused-padding = 2;
        label-focused-margin = 1;
        label-focused-font = 1;
        label-focused-foreground = color.foreground;
        label-focused-background = color.cyan;
        label-focused-overline = color.pink;
        label-focused-underline = color.pink;
        label-unfocused = "%index%";
        label-unfocused-padding = 2;
        label-unfocused-margin = 1;
        label-unfocused-background = color.darkGray;
        label-unfocused-foreground = color.foreground;
        label-unfocused-overline = color.blue;
        label-unfocused-underline = color.blue;
        label-unfocused-font = 1;
        label-urgent = "%index%";
        label-urgent-padding = 2;
        label-urgent-margin = 1;
        label-urgent-font = 1;
        label-visible = "%index%";
        label-visible-padding = 2;
        label-visible-margin = 1;
        label-visible-font = 1;
        index-sort = true;
      };
      "module/xwindow" = {
        type = "internal/xwindow";
        label-font = 1;
      };
      "module/cpu" = {
        type = "internal/cpu";
        interval = "0.5";
        format = "<label> <ramp-load>";
        format-background = color.cyan;
        format-foreground = color.darkGray;
        format-underline = color.cyan;
        format-overline = color.cyan;
        format-padding = 2;
        label = "";
        label-font = 1;
        ramp-load-0 = "▁";
        ramp-load-0-font = 1;
        ramp-load-0-foreground = color.darkGray;
        ramp-load-1 = "▂";
        ramp-load-1-font = 1;
        ramp-load-1-foreground = color.darkGray;
        ramp-load-2 = "▃";
        ramp-load-2-font = 1;
        ramp-load-2-foreground = color.darkGray;
        ramp-load-3 = "▄";
        ramp-load-3-font = 1;
        ramp-load-3-foreground = color.darkGray;
        ramp-load-4 = "▅";
        ramp-load-4-font = 1;
        ramp-load-4-foreground = color.foreground;
        ramp-load-5 = "▆";
        ramp-load-5-font = 1;
        ramp-load-5-foreground = color.foreground;
        ramp-load-6 = "▇";
        ramp-load-6-font = 1;
        ramp-load-6-foreground = color.pink;
        ramp-load-7 = "█";
        ramp-load-7-font = 1;
        ramp-load-7-foreground = color.pink;
      };
      "module/memory" = {
        type = "internal/memory";
        format = "<label> <ramp-used>";
        label-font = 1;
        format-padding = 2;
        format-background = color.purple;
        format-foreground = color.darkGray;
        format-underline = color.purple;
        format-overline = color.purple;
        label = "";
        ramp-used-0 = "▁";
        ramp-used-0-font = 1;
        ramp-used-0-foreground = color.darkGray;
        ramp-used-1 = "▂";
        ramp-used-1-font = 1;
        ramp-used-1-foreground = color.darkGray;
        ramp-used-2 = "▃";
        ramp-used-2-font = 1;
        ramp-used-2-foreground = color.darkGray;
        ramp-used-3 = "▄";
        ramp-used-3-font = 1;
        ramp-used-3-foreground = color.darkGray;
        ramp-used-4 = "▅";
        ramp-used-4-font = 1;
        ramp-used-4-foreground = color.foreground;
        ramp-used-5 = "▆";
        ramp-used-5-font = 1;
        ramp-used-5-foreground = color.foreground;
        ramp-used-6 = "▇";
        ramp-used-6-font = 1;
        ramp-used-6-foreground = color.pink;
        ramp-used-7 = "█";
        ramp-used-7-font = 1;
        ramp-used-7-foreground = color.pink;
      };
      "module/clock" = {
        type = "internal/date";
        format-padding = 2;
        label = "%time% %date%";
        time = " %H:%M %p";
        date = " %Y-%m-%d";
        format-background = color.pink;
        format-foreground = color.darkGray;
        format-underline = color.pink;
        format-overline = color.pink;
      };
      "module/battery" = {
        type = "internal/battery";
        battery = "BAT0";
        adapter = "AC";
        full-at = 96;

        time-format = "%H:%M";
        format-charging = "<animation-charging> <label-charging>";
        label-charging = "%percentage%%";
        format-discharging = "<ramp-capacity> <label-discharging>";
        label-discharging = "%percentage%%";

        format-full = "<label-full>";
        format-full-prefix = " ";
        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
        ramp-capacity-0-foreground = color.red;
        ramp-capacity-foreground = color.foreground;
        bar-capacity-width = 10;
        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";

        animation-charging-framerate = 750;
      };
      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        use-ui-max = true;
        interval = 5;

        format-volume = "<ramp-volume> <label-volume>";
        format-volume-background = color.green;
        format-volume-foreground = color.darkGray;
        format-volume-underline = color.green;
        format-volume-overline = color.green;
        format-volume-padding = 2;

        label-muted = " mute";
        label-foreground = color.darkGray;

        label-volume = "%percentage%%";
        label-volume-foreground = color.darkGray;
        label-muted-foreground = color.darkGray;

        ramp-volume-0 = "";
        ramp-volume-1 = "";
        ramp-volume-2 = "";
        ramp-font = 1;
      };
    };
    script = "polybar main &";
  };

  xsession.windowManager.i3 = let
    modifier = "Mod4";
  in {
    enable = true;
    config = {
      bars = [];
      colors = {
        focused = { background = color.pink; border = color.pink; childBorder = color.darkGray; indicator = color.pink; text = color.foreground; };
        unfocused = { background = color.background; border = color.background; childBorder = color.foreground; indicator = color.pink; text = color.foreground; };
        focusedInactive = { background = color.background; border = color.background; childBorder = color.pink; indicator = color.pink; text = color.foreground; };
        urgent = { background  = color.yellow; border  = color.yellow; childBorder = color.darkGray; indicator = color.pink; text = color.foreground; };
        placeholder = { background = color.background; border = color.background; childBorder = color.background; indicator = color.background; text = color.foreground; };
      };
      fonts = [ monospaceFont "FontAwesome 12" ];
      keybindings = lib.mkOptionDefault {
        "${modifier}+Shift+Return" = "exec alacritty";
        "${modifier}+Return" = "exec alacritty";
        "${modifier}+p" = ''exec "\${pkgs.rofi}/bin/rofi -show combi -lines 6"'';
        "${modifier}+x" = "kill";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+Left" = "focus left";
        "${modifier}+Down" = "focus down";
        "${modifier}+Up" = "focus up";
        "${modifier}+Right" = "focus right";
        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Tab" = "workspace back_and_forth";
        "${modifier}+q" = "reload";
        "${modifier}+Shift+r" = "restart";
        "${modifier}+Shift+q" = ''exec "i3-nagbar -t warning -m 'Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'"'';
        "${modifier}+Shift+z" = ''exec "mate-screensaver-command -l"'';
        "${modifier}+r" = "mode resize";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+s" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+t" = "layout toggle all";
        "${modifier}+Shift+space" = "floating toggle";
        "${modifier}+space" = "focus mode_toggle";
        "${modifier}+Ctrl+4" = ''exec "--no-startup-id mate-screenshot --interactive"'';
        "${modifier}+a" = "focus parent";
        "${modifier}+d" = "focus child";
        "XF86AudioRaiseVolume" = ''exec "amixer -q set Master 5%+"'';
        "XF86AudioLowerVolume" = ''exec "amixer -q set Master 5%-"'';
      };
      gaps = {
        inner = 4;
        outer = 4;
        smartBorders = "on";
      };
      modes = {
        resize = {
          "h" = "resize shrink width 10 px or 10 ppt";
          "j" = "resize grow height 10 px or 10 ppt";
          "k" = "resize shrink height 10 px or 10 ppt";
          "l" = "resize grow width 10 px or 10 ppt";

          "Left" = "resize shrink width 10 px or 10 ppt";
          "Down" = "resize grow height 10 px or 10 ppt";
          "Up" = "resize shrink height 10 px or 10 ppt";
          "Right" = "resize grow width 10 px or 10 ppt";

          "Return" = "mode default";
          "Escape" = "mode default";
          "${modifier}+r" = "mode default";
        };
      };
      modifier = modifier;
      startup = [
        { command = "compton -b"; notification = false; }
        { command = "systemctl --user restart polybar"; always = true; notification = false; }
        { command = "nm-applet"; notification = false; }
      ];
    };
    package = pkgs.i3-gaps;
  };
}
