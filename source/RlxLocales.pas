unit RlxLocales;

///////////////////////////////////////////////////////////////////
///  Delphi Relax: http://code.marcocantu.com/p/delphirelax     ///
///                                                             ///
///  Project coordination: Marco Cantu', marco.cantu@gmail.com  ///
///                                                             ///
///  Source code available under MPL http://www.mozilla.org/MPL ///
///                                                             ///
///////////////////////////////////////////////////////////////////
///                                                             ///
/// Contributors to this unit:                                  ///
///   Marco Cantu                                               ///
///                                                             ///
///////////////////////////////////////////////////////////////////

interface

uses
  Classes, SysUtils, Generics.Collections;

// data shared among RlxRazor and other units, to avoid Razor to depend on
// DataSnap and enterprise features

var
  RlxLocaleDictionary: TDictionary <integer, TFormatSettings>;

implementation

initialization
  RlxLocaleDictionary := TDictionary <integer, TFormatSettings>.Create;

finalization
  FreeAndNil(RlxLocaleDictionary);

end.
