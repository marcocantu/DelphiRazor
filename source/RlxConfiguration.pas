unit RlxConfiguration;

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
///   Francesco Fontana, Marco Cantu                            ///
///                                                             ///
///////////////////////////////////////////////////////////////////

interface

uses
  IniFiles, Classes, SysUtils;

type
  TRlxConfiguration = class
  private
    memIni: TMemIniFile;
    fDatabaseParams: TStrings;
    function GetValue(const Name: string): string;
  public
    constructor CreateFromIni (const filename: string);
    destructor Destroy; override;

    // read section:value from ini file
    property Value[const Name: string]: string read GetValue; default;
    procedure ReadSectionValues (const sectionName: string; sList: TStrings);

    function JSONDateTimeFormatString: string;
    function DateFormatString: string;
    function DateTimeFormatString: string;
  end;

// Warning: Since this is global we might have to figure out if
// it needs thread protection, although is is basically a read-only structure
var
  ServerConf: TRlxConfiguration = nil;

implementation

{ TConfiguration }

constructor TRlxConfiguration.CreateFromIni(const filename: string);
begin
  memIni := TMemIniFile.Create (filename);
  fDatabaseParams := TStringList.Create;
end;

function TRlxConfiguration.DateFormatString: string;
begin
  Result := GetValue ('database:DateFormatString');
  if Result = '' then
    Result := FormatSettings.ShortDateFormat;
end;

function TRlxConfiguration.DateTimeFormatString: string;
begin
  Result := GetValue ('database:DateTimeFormatString');
  if Result = '' then
    Result := FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat;
end;

destructor TRlxConfiguration.Destroy;
begin
  fDatabaseParams.Free;
  memIni.Free;
  inherited;
end;

function TRlxConfiguration.GetValue(const Name: string): string;
var
  nPos: Integer;
begin
  nPos := Pos (':', Name);
  Result := memIni.ReadString(
    Copy (Name, 1, nPos -1),
    Copy (Name, nPos + 1), '');
end;

function TRlxConfiguration.JSONDateTimeFormatString: string;
begin
  Result := GetValue ('database:JSONDateTimeFormatString');
  if Result = '' then
    Result := FormatSettings.ShortDateFormat;
end;

procedure TRlxConfiguration.ReadSectionValues(const sectionName: string;
  sList: TStrings);
begin
  memIni.ReadSectionValues(sectionName, sList);
end;

initialization
  ServerConf := TRlxConfiguration.CreateFromIni(
    ChangeFileExt (ParamStr(0), '.ini'));


end.
