This changelog document (CHANGELOG.md) is a summary of all notable changes to the [InsideBlue BLE Tool](https://github.com/eriklins/InsideBlue-BLE-Tool) project and application. 

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and uses the Markdown formatting syntax. You can find the original example and template [here](https://github.com/olivierlacan/keep-a-changelog/blob/main/CHANGELOG.md).



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



[0.4]: https://github.com/eriklins/InsideBlue-BLE-Tool/compare/v0.3...v0.4
[0.3]: https://github.com/eriklins/InsideBlue-BLE-Tool/releases/tag/v0.3
