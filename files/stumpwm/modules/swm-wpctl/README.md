# StumpWM wpctl (swm-wpctl)

Minimalistic PipeWire volume and microphone control module for StumpWM.

## Requirements

- [wireplumber](https://github.com/PipeWire/wireplumber) installed
- [parse-float](https://github.com/soemraws/parse-float) installed

## Installation

```bash
cd ~/.stumpwm.d/modules/
git clone https://github.com/Junker/stumpwm-wpctl wpctl
```

```lisp
(stumpwm:add-to-load-path "~/.stumpwm.d/modules/wpctl")
(load-module "wpctl")
```

## Usage

```lisp
  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "wpctl-volume-up")
  (define-key *top-map* (kbd "XF86AudioLowerVolume") "wpctl-volume-down")
  (define-key *top-map* (kbd "XF86AudioMute") "wpctl-toggle-mute")
```

### Commands

- `wpctl-volume-up`
- `wpctl-volume-down`
- `wpctl-mute`
- `wpctl-unmute`
- `wpctl-toggle-mute`
- `wpctl-set-volume volume`

**control of source, e.g. microphone:**

- `wpctl-source-volume-up`
- `wpctl-source-volume-down`
- `wpctl-source-mute`
- `wpctl-source-unmute`
- `wpctl-source-toggle-mute`
- `wpctl-source-set-volume volume`

### Parameters

- `wpctl:*step*` - volume increase/decrease step
- `wpctl:*mixer-command*` - external mixer program that opens on modeline right click (default: pavucontrol)
- `wpctl:*default-sink-id*`
- `wpctl:*default-source-id*`
- `wpctl:*wpctl-path*`

## Modeline

`%P` - wpctl formatter

### Parameters for modeline

- `wpctl:*modeline-fmt*` - format of wpctl modeline (default: "%b(%v)")
  - `%b` - volume bar
  - `%v` - volume value

### Modeline mouse interaction

- **left button:** mute/unmute
- **right button:** open mixer program
- **wheel up:** volume up
- **wheel down:** volume down
