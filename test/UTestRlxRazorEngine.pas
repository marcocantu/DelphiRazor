unit UTestRlxRazorEngine;

interface

uses
  TestFramework,
  RlxRazor;

type
  TTestRlxRazorEngine = class(TTestCase)
  private
    FRlxRazorEngine: TRlxRazorEngine;
    FWarning: string;
    procedure DoValue(Sender: TObject;
                      const ObjectName, FieldName: string;
                      var ReplaceText: string);
    procedure DoLang(Sender: TObject;
                     const FieldName: string;
                     var ReplaceText: string);
    procedure DoScaffolding(Sender: TObject;
                            const qualifClassName: string;
                            var ReplaceText: string);
    procedure DoWarn(Sender: TObject;
                     const strWarning: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetTemplatesFolder;
    procedure TestGetOtherValue;
    procedure TestGetLang;
    procedure TestGetScaffolding;
    procedure TestAddWarning;
    procedure TestGetLangFormats;
    procedure TestLocaleDictionary;
    procedure TestAddToDictionary;
    procedure TestInDictionary;
    procedure TestDataObjects;
  end;

implementation

uses
  SysUtils,
  RlxLocales;

const
  cObjectName = 'TEmployee';
  cFieldName = 'First_Name';
  cFieldNameGerman = 'Vorname';
  cReplaceText = 'Roberto';
  cUnknown = 'Unkown';
  cWarning = 'Hire Date is missing';

type
  TEmployee = class
  private
    FJOB_CODE: string;
    FJOB_GRADE: SmallInt;
    FEMP_NO: SmallInt;
    FDEPT_NO: string;
    FFIRST_NAME: string;
    FJOB_COUNTRY: string;
    FLAST_NAME: string;
    FSALARY: string;
    FHIRE_DATE: TDateTime;
    FPHONE_EXT: string;
  public
    property EMP_NO: SmallInt read FEMP_NO write FEMP_NO;
    property FIRST_NAME: string read FFIRST_NAME write FFIRST_NAME;
    property LAST_NAME: string read FLAST_NAME write FLAST_NAME;
    property PHONE_EXT: string read FPHONE_EXT write FPHONE_EXT;
    property HIRE_DATE: TDateTime read FHIRE_DATE write FHIRE_DATE;
    property DEPT_NO: string read FDEPT_NO write FDEPT_NO;
    property JOB_CODE: string read FJOB_CODE write FJOB_CODE;
    property JOB_GRADE: SmallInt read FJOB_GRADE write FJOB_GRADE;
    property JOB_COUNTRY: string read FJOB_COUNTRY write FJOB_COUNTRY;
    property SALARY: string read FSALARY write FSALARY;
  end;


{ TTestRlxRazorEngine }

procedure TTestRlxRazorEngine.DoLang(Sender: TObject;
                                     const FieldName: string;
                                     var ReplaceText: string);
begin
  if FieldName = cFieldName then
    ReplaceText := cFieldNameGerman
  else
    ReplaceText := cUnknown;
end;

procedure TTestRlxRazorEngine.DoScaffolding(Sender: TObject;
                                            const qualifClassName: string;
                                            var ReplaceText: string);
begin
  if qualifClassName = Format('%s.%s', [cObjectName, cFieldName]) then
    ReplaceText := cReplaceText
  else
    ReplaceText := cUnknown;
end;

procedure TTestRlxRazorEngine.DoValue(Sender: TObject;
                                      const ObjectName, FieldName: string;
                                      var ReplaceText: string);
begin
  if (ObjectName = cObjectName) and (FieldName = cFieldName) then
    ReplaceText := cReplaceText
  else
    ReplaceText := cUnknown;
end;

procedure TTestRlxRazorEngine.DoWarn(Sender: TObject;
                                     const strWarning: string);
begin
  FWarning := strWarning;
end;

procedure TTestRlxRazorEngine.SetUp;
begin
  inherited;
  FRlxRazorEngine := TRlxRazorEngine.Create(nil);
end;

procedure TTestRlxRazorEngine.TearDown;
begin
  FRlxRazorEngine.Free;
  inherited;
end;

procedure TTestRlxRazorEngine.TestAddToDictionary;
var
  Employee: TEmployee;
  DictItem: TRazDictItem;
begin
  Employee := TEmployee.Create;
  try
    CheckEquals(0, FRlxRazorEngine.DataObjects.Count, 'DataObjects.Count');
    FRlxRazorEngine.AddToDictionary('employee', Employee, False);
    CheckEquals(1, FRlxRazorEngine.DataObjects.Count, 'DataObjects.Count');
    CheckTrue(FRlxRazorEngine.DataObjects.ContainsKey('employee'), 'DataObjects.ContainsKey(''employee'')');
    FRlxRazorEngine.DataObjects.TryGetValue('employee', DictItem);
    CheckIs(DictItem.TheObject, TEmployee, 'DictItem.TheObject');
  finally
    Employee.Free;
  end;
end;

procedure TTestRlxRazorEngine.TestAddWarning;
begin
  FWarning := EmptyStr;
  FRlxRazorEngine.OnWarn := DoWarn;
  FRlxRazorEngine.AddWarning(cWarning);
  CheckEquals(cWarning, FWarning, 'Warning');
end;

procedure TTestRlxRazorEngine.TestCreate;
begin
  CheckEquals(0, RlxLocaleDictionary.Count, 'LocaleDictionary.Count');
  CheckEquals(0, FRlxRazorEngine.DataObjects.Count, 'DataObjects.Count');
  CheckEquals(Ord(ddIgnore), Ord(FRlxRazorEngine.DictionaryDuplicates), 'DictionaryDuplicates');
end;

procedure TTestRlxRazorEngine.TestDataObjects;
begin
  TestAddToDictionary;
end;

procedure TTestRlxRazorEngine.TestGetLang;
var
  sReplaceText: string;
begin
  FRlxRazorEngine.OnLang := DoLang;
  sReplaceText := FRlxRazorEngine.GetLang(cFieldName);
  CheckEquals(cFieldNameGerman, sReplaceText, 'ReplaceText');
  sReplaceText := FRlxRazorEngine.GetLang(cUnknown);
  CheckEquals(cUnknown, sReplaceText, 'ReplaceText');
end;

procedure TTestRlxRazorEngine.TestGetLangFormats;
const
  cGermanLanguageID = 1;
var
  FormatResult, GermanFormatSettings: TFormatSettings;
begin
  FormatResult := FRlxRazorEngine.GetLangFormats(cGermanLanguageID);
  CheckEquals(FormatSettings.DateSeparator, FormatResult.DateSeparator, 'DateSeparator');
  CheckEquals(FormatSettings.ShortDateFormat, FormatResult.ShortDateFormat, 'ShortDateFormat');
  GermanFormatSettings.DateSeparator := '.';
  GermanFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  RlxLocaleDictionary.Add(cGermanLanguageID, GermanFormatSettings);
  FormatResult := FRlxRazorEngine.GetLangFormats(cGermanLanguageID);
  CheckEquals(GermanFormatSettings.DateSeparator, FormatResult.DateSeparator, 'DateSeparator');
  CheckEquals(GermanFormatSettings.ShortDateFormat, FormatResult.ShortDateFormat, 'ShortDateFormat');
end;

procedure TTestRlxRazorEngine.TestGetOtherValue;
var
  sReplaceText: string;
begin
  FRlxRazorEngine.OnValue := DoValue;
  sReplaceText := FRlxRazorEngine.GetOtherValue(cObjectName, cFieldName);
  CheckEquals(cReplaceText, sReplaceText, 'ReplaceText');
  sReplaceText := FRlxRazorEngine.GetOtherValue(cObjectName, cUnknown);
  CheckEquals(cUnknown, sReplaceText, 'ReplaceText');
end;

procedure TTestRlxRazorEngine.TestGetScaffolding;
var
  sReplaceText: string;
begin
  FRlxRazorEngine.OnScaffolding := DoScaffolding;
  sReplaceText := FRlxRazorEngine.GetScaffolding(Format('%s.%s', [cObjectName, cFieldName]));
  CheckEquals(cReplaceText, sReplaceText, 'ReplaceText');
  sReplaceText := FRlxRazorEngine.GetScaffolding(Format('%s.%s', [cObjectName, cUnknown]));
  CheckEquals(cUnknown, sReplaceText, 'ReplaceText');
end;

procedure TTestRlxRazorEngine.TestGetTemplatesFolder;
const
  cFilesFolder = 'c:\FilesFolder\';
  cTemplatesFolder = 'c:\TemplatesFolder\';
begin
  FRlxRazorEngine.FilesFolder := cFilesFolder;
  CheckEquals(cFilesFolder, FRlxRazorEngine.GetTemplatesFolder, 'GetTemplatesFolder');
  FRlxRazorEngine.TemplatesFolder := cTemplatesFolder;
  CheckEquals(cTemplatesFolder, FRlxRazorEngine.GetTemplatesFolder, 'GetTemplatesFolder');
end;

procedure TTestRlxRazorEngine.TestInDictionary;
var
  Employee: TEmployee;
  DictItem: TRazDictItem;
begin
  Employee := TEmployee.Create;
  try
    CheckEquals(0, FRlxRazorEngine.DataObjects.Count, 'DataObjects.Count');
    FRlxRazorEngine.AddToDictionary('employee', Employee, False);
    CheckEquals(1, FRlxRazorEngine.DataObjects.Count, 'DataObjects.Count');
    CheckTrue(FRlxRazorEngine.InDictionary('employee'), 'InDictionary(''employee'')');
  finally
    Employee.Free;
  end;
end;

procedure TTestRlxRazorEngine.TestLocaleDictionary;
const
  cGermanLanguageID = 2;
var
  FormatResult, GermanFormatSettings: TFormatSettings;
begin
  // it is now a global structure, so there are already some values in it
  // CheckEquals(0, RlxLocaleDictionary.Count, 'LocaleDictionary.Count');
  GermanFormatSettings.DateSeparator := '.';
  GermanFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  RlxLocaleDictionary.Add(cGermanLanguageID, GermanFormatSettings);
  CheckTrue(RlxLocaleDictionary.ContainsKey(cGermanLanguageID), 'LocaleDictionary.ContainsKey(cGermanLanguageID)');
  RlxLocaleDictionary.TryGetValue(cGermanLanguageID, FormatResult);
  CheckEquals(GermanFormatSettings.DateSeparator, FormatResult.DateSeparator, 'DateSeparator');
  CheckEquals(GermanFormatSettings.ShortDateFormat, FormatResult.ShortDateFormat, 'ShortDateFormat');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestRlxRazorEngine.Suite);

end.
