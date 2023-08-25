This changelog document (CHANGELOG.md) is a summary of all notable changes to the [InsideBlue BLE Tool](https://github.com/eriklins/InsideBlue-BLE-Tool) project and application. 

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and uses the Markdown formatting syntax. You can find the original example and template [here](https://github.com/olivierlacan/keep-a-changelog/blob/main/CHANGELOG.md).


## [0.7.3] - 2023-08-25

### Added

- None.

### Changed

- Slight optimization for scaling on high DPI desktops.

### Fixed

- None.


## [0.7.2] - 2023-08-23

### Added

- Setting of ModemIn characteristic when starting VSP terminal.
- Handling of ModemOut characteristic notifications and enabling/disabling of Send button accordingly.

### Changed

- None.

### Fixed

- Typo in UartTerminalStart() with ModemIn/Out variable names.


## [0.7.1] - 2023-08-16

### Added

- None.

### Changed

- Improved garbage collection of device form data after disconnect.

### Fixed

- Restore of "Connect" button in scan window when connected to multiple devices.


## [0.7] - 2023-08-10

### Added

- Added BT style application icon and form window icon instead of default Lazarus one.

### Changed

- Welcome message.
- Updated BT assigned numbers list.
- Updated underlying SimpleBLE library to 0.6.2

### Fixed

- Fixed issue with scanning not catching all manufaturer specific data fields in advert payload.
- Fixed restore of main form window when has been resized before.


## [0.6.1] - 2023-03-28

### Added

- Updated simpleble.pas with SimpleBleGetVersion().
- Added output of SimpleBLE version to log on start.
- Added checkbox in vsp terminal to select write command vs. write request (if exposed by peripheral).

### Changed

- None.

### Fixed

- Reduced flickering in VSP terminal when receiving large chunks of data.
- Fixed bug when searching VSP service which is also an assigned service (didn't show vsp terminal button).

### Removed

- None.


## [0.6] - 2023-02-28

### Added

- Added check for the device connect window height to not exceed desktop in case of a large GATT table.
- Added closing curly bracket as possible line endings on VSP terminal (useful for e.g. JSON data).
- Added support for the u-blox u-connectXpress (formerly uCS) BLE Serial Port Service.
- Added check for the position of next window to not exceed desktop.

### Changed

- If no GATT characteristics are found for ModemIn and ModemOut, VSP terminal will continue without.
- Changed the VSP terminal font to a more monospace-like terminal font. Source Code Pro is now used.
- The VSP terminal can now be resized and automatically closes when a BLE device is disconnected.

### Fixed

- Fixed MAC address on VSP terminal. The MAC address is now displayed correctly in upper case format (instead of lower case).
- Fixed attempt to unsubscribe from notifications and indications when subscription failed before (still not fully working).
- Fixed form issues in the scan window (missing label for manufacturer data, duplicate text box for manufacturer data).

### Removed

- Removed counting of active VSP terminals, not needed.


## [0.5] - 2023-02-24

### Added

- Added MTU size to device connect window and form. Longer data strings are now split into multiple BLE writes.
- Added visual effect (blinking) to the text data field for received notifications via color change function.

### Changed

- VSP terminal now uses MTU size -3 as presumption for the RX characteristic max length. This value can also be set manually.

### Fixed
- Fixed bug when subscribing to notifications or indications on non-VSP services / characteristics.

### Removed

- Removed redundant code in tick timer for disconnect.


## [0.4] - 2023-02-23

### Added

- Added parts of the VSP terminal, started implementing the paired device status (not yet supported in the SimpleBLE library).
- Added sanity check to not allow VSP terminal when one of the corresponding characteristics have been notified or indicated.
- Added proprietary UART services and characteristics (Laird VSP plus Nordic NUS) and functions to map their UUIDs to names.
- Added service data to scan output as support for this has been recently added to the SimpleBLE library.

### Changed

- Changed storage and handling of UUIDs (TSimpleBleUuid instead of strings). This affected multiple functions, but it is simpler in the end.
- Aligned code for SimpleBLE v0.6.0 release. Display service data in the scan window and make look more consistent.
- Some UI tweaking for the GNU/Linux OS, also updated the BtAssignedNumbersToPascalUnit.py accordingly.
- Code reorganization and independency of units. Restructured UART terminal functions.

### Fixed
- Fixed bug with resizing the connect window, and resolved cross-dependencies between units.
- Fixed position of the next form (still not perfect when opening or closing other forms).
- Fixed crash on exit when UART terminal was open. Fixed crash on help button.
- Fixed bug not finding 16-bit UUIDs after last recent changes.


## [0.3] - 2023-02-15

Initial release on GitHub under https://github.com/eriklins/InsideBlue-BLE-Tool.



[Unreleased]: https://github.com/eriklins/InsideBlue-BLE-Tool/compare/v0.6...HEAD
[0.6]: https://github.com/eriklins/InsideBlue-BLE-Tool/compare/v0.5...v0.6
[0.5]: https://github.com/eriklins/InsideBlue-BLE-Tool/compare/v0.4...v0.5
[0.4]: https://github.com/eriklins/InsideBlue-BLE-Tool/compare/v0.3...v0.4
[0.3]: https://github.com/eriklins/InsideBlue-BLE-Tool/releases/tag/v0.3
