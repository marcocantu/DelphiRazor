﻿unit TestRlxRazor;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, DB, Generics.Collections, CopyPrsr, Classes, Contnrs, SysUtils, HTTPProd,
  RlxRazor, HTTPApp;

type
  // Test methods for class TRlxRazorEngine

  TestTRlxRazorEngine = class(TTestCase)
  strict private
    FRlxRazorEngine: TRlxRazorEngine;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetTemplatesFolder;
    procedure TestGetOtherValue;
    procedure TestGetLang;
    procedure TestGetScaffolding;
    procedure TestAddWarning;
    procedure TestProcessRequest;
    procedure TestGetLangFormats;

    // 5. templates
  end;
  // Test methods for class TRlxRazorProcessor

  TestTRlxRazorProcessor = class(TTestCase)
  strict private
    FRlxRazorProcessor: TRlxRazorProcessor;
  private
    procedure TestValueHandler (Sender: TObject; const ObjectName: string;
      const FieldName: string; var ReplaceText: string);
    procedure RunLoginRequiredScript;
    procedure RunLoginPermissionScript;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // 1. basic processing
    procedure TestDoBlock;
    procedure TestDoubleAt;
    procedure TestUnicode;

    // 2. values and dictionary
    procedure TestValueEvent;
    procedure TestAddToDictionary;
    procedure TestValueFromDictionary;
    // add test for db data access

    // 3. if
    procedure TestIfTrue;
    procedure TestIfFalse;
    procedure TestIfEmptyString;
    procedure TestIfNotEmptyString;
    // add tests for booelan evaluation of numeric values

    // 4. foreach
    procedure TestForEachList;
    procedure TestForEachDataset;
    procedure TestForEachNestedDatasets;

    // 5. lang

    // 6. login stuff
    procedure TestIsLogged;
    procedure TestIsNotLogged;
    procedure TestPermission;
    procedure TestWrongPermission;

  end;

implementation

uses
  FireDAC.Comp.Client;

const
  SampleObjectName = 'AnObject';
  SampleFieldName = 'AField';
  SampleFieldValue = 'AValue';
  SampleBoolFieldName = 'ABool';

type
  TSimpleObj = class
  private
    FAField: string;
    FABool: Boolean;
    procedure SetAField(const Value: string);
    procedure SetABool(const Value: Boolean);
  public
    property AField: string read FAField write SetAField;
    property ABool: Boolean read FABool write SetABool;
  end;



procedure TestTRlxRazorEngine.SetUp;
begin
  FRlxRazorEngine := TRlxRazorEngine.Create (nil);
end;

procedure TestTRlxRazorEngine.TearDown;
begin
  FRlxRazorEngine.Free;
  FRlxRazorEngine := nil;
end;

procedure TestTRlxRazorEngine.TestGetTemplatesFolder;
var
  ReturnValue: string;
begin
  // ReturnValue := FRlxRazorEngine.GetTemplatesFolder;
  // TODO: Validate method results
end;

procedure TestTRlxRazorEngine.TestGetOtherValue;
var
  ReturnValue: string;
  FieldName: string;
  ObjectName: string;
begin
  // TODO: Setup method call parameters
  // ReturnValue := FRlxRazorEngine.GetOtherValue(ObjectName, FieldName);
  // TODO: Validate method results
end;

procedure TestTRlxRazorEngine.TestGetLang;
var
  ReturnValue: string;
  FieldName: string;
begin
  // TODO: Setup method call parameters
  // ReturnValue := FRlxRazorEngine.GetLang(FieldName);
  // TODO: Validate method results
end;

procedure TestTRlxRazorEngine.TestGetScaffolding;
var
  ReturnValue: string;
  strDottedClass: string;
begin
  // TODO: Setup method call parameters
  // ReturnValue := FRlxRazorEngine.GetScaffolding(strDottedClass);
  // TODO: Validate method results
end;

procedure TestTRlxRazorEngine.TestAddWarning;
var
  strWarning: string;
begin
  // TODO: Setup method call parameters
  FRlxRazorEngine.AddWarning(strWarning);
  // TODO: Validate method results
end;

procedure TestTRlxRazorEngine.TestProcessRequest;
var
  ReturnValue: string;
  userRoles: string;
  inFolder: string;
  LanguageID: Integer;
  LoggedUser: Boolean;
  Found: Boolean;
  Request: TWebRequest;
begin
  // TODO: Setup method call parameters
  //ReturnValue := FRlxRazorEngine.ProcessRequest(Request, Found, LoggedUser, LanguageID,
  //    inFolder, userRoles);
  // TODO: Validate method results
end;

procedure TestTRlxRazorEngine.TestGetLangFormats;
var
  ReturnValue: TFormatSettings;
  language_id: Integer;
begin
  // TODO: Setup method call parameters
  // ReturnValue := FRlxRazorEngine.GetLangFormats(language_id);
  // TODO: Validate method results
end;

procedure TestTRlxRazorProcessor.RunLoginPermissionScript;
var
  strBlock: string;
begin
  strBlock := 'Something @LoginRequired.MyGroup else';
  strBlock := FRlxRazorProcessor.DoBlock(strBlock);
end;

procedure TestTRlxRazorProcessor.RunLoginRequiredScript;
var
  strBlock: string;
begin
  strBlock := 'Something @LoginRequired else';
  strBlock := FRlxRazorProcessor.DoBlock(strBlock);
end;

procedure TestTRlxRazorProcessor.SetUp;
begin
  FRlxRazorProcessor := TRlxRazorProcessor.Create (nil);
end;

procedure TestTRlxRazorProcessor.TearDown;
begin
  FRlxRazorProcessor.Free;
  FRlxRazorProcessor := nil;
end;

procedure TestTRlxRazorProcessor.TestAddToDictionary;
var
  simpleObj: TSimpleObj;
begin
  // add non-owned object to processor
  simpleObj := TSimpleObj.Create;
  try
    FRlxRazorProcessor.AddToDictionary(SampleObjectName, SimpleObj, False);
    CheckEquals (FRlxRazorProcessor.DataObjects.Count, 1);
    CheckEquals (FRlxRazorProcessor.InDictionary (SampleObjectName), True);
    FRlxRazorProcessor.DataObjects.Remove(SampleObjectName);
    CheckEquals (FRlxRazorProcessor.DataObjects.Count, 0);
  finally
    simpleObj.Free;
  end;
end;

procedure TestTRlxRazorProcessor.TestDoBlock;
var
  ReturnValue: string;
  strBlock: string;
begin
  // generic text should remain the same after processing
  strBlock := 'Anything';
  ReturnValue := FRlxRazorProcessor.DoBlock(strBlock);
  CheckEquals (strBlock, ReturnValue);
end;

procedure TestTRlxRazorProcessor.TestDoubleAt;
var
  ReturnValue: string;
  strBlock: string;
begin
  // double @@ shoudl be returned as single @
  strBlock := 'Anything @something';
  ReturnValue := FRlxRazorProcessor.DoBlock(DoubleAtSymbol (strBlock));
  CheckEquals (strBlock, ReturnValue);
end;

procedure TestTRlxRazorProcessor.TestForEachList;
var
  list: TList<TObject>;
  simpleObj: TSimpleObj;
  strBlock: string;
  ReturnValue: string;

begin
  // create a list with 2 objects
  list := TList<TObject>.Create; //owns objects
  simpleObj := TSimpleObj.Create;
  simpleObj.AField := 'one';
  list.Add(simpleObj);
  simpleObj := TSimpleObj.Create;
  simpleObj.AField := 'two';
  list.Add(simpleObj);
  try
    FRlxRazorProcessor.AddToDictionary('list', list, False);

    strBlock := '@foreach (var item in list) {@item.AField,}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('one, two,', ReturnValue);

    FRlxRazorProcessor.DataObjects.Remove('list');
  finally
    list.Free;
  end;

end;

procedure TestTRlxRazorProcessor.TestForEachDataset;
var
  strBlock: string;
  ReturnValue: string;
  LDataset: TFDMemTable;
  I: Integer;
begin
  LDataset := TFDMemTable.Create(nil);
  try
    LDataset.FieldDefs.Add('Name', ftString, 30, True);
    LDataset.Open;
    for I := 1 to 3 do
    begin
      LDataset.Append;
      LDataset.FieldByName('Name').AsString := 'Record' + IntToStr(I);
      LDataset.Post;
    end;

    FRlxRazorProcessor.AddToDictionary('dataset', LDataset, False);

    strBlock := '@foreach (var item in dataset) {@item.Name,}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('Record1, Record2, Record3,', ReturnValue);

    FRlxRazorProcessor.DataObjects.Remove('dataset');
  finally
    LDataset.Free;
  end;

end;

procedure TestTRlxRazorProcessor.TestForEachNestedDatasets;
var
  strBlock: string;
  ReturnValue: string;
  LDatasetCat: TFDMemTable;
  LDatasetRec: TFDMemTable;
  I: Integer;
begin
  LDatasetCat := TFDMemTable.Create(nil);
  LDatasetRec := TFDMemTable.Create(nil);
  try
    LDatasetCat.FieldDefs.Add('Name', ftString, 30, True);
    LDatasetCat.Open;
    for I := 1 to 2 do
    begin
      LDatasetCat.Append;
      LDatasetCat.FieldByName('Name').AsString := 'Category' + IntToStr(I);
      LDatasetCat.Post;
    end;

    LDatasetRec.FieldDefs.Add('Name', ftString, 30, True);
    LDatasetRec.Open;
    for I := 1 to 3 do
    begin
      LDatasetRec.Append;
      LDatasetRec.FieldByName('Name').AsString := 'Record' + IntToStr(I);
      LDatasetRec.Post;
    end;

    FRlxRazorProcessor.AddToDictionary('datasetCat', LDatasetCat, False);
    FRlxRazorProcessor.AddToDictionary('datasetRec', LDatasetRec, False);

    strBlock := '@foreach (var cat in datasetCat) {@cat.Name: @foreach (var rec in datasetRec) {@rec.Name,}}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('Category1:      Record1, Record2, Record3, Category2:      Record1, Record2, Record3,', ReturnValue);

    FRlxRazorProcessor.DataObjects.Remove('datasetRec');
    FRlxRazorProcessor.DataObjects.Remove('datasetCat');
  finally
    LDatasetRec.Free;
    LDatasetCat.Free;
  end;

end;

procedure TestTRlxRazorProcessor.TestIfEmptyString;
var
  ReturnValue: string;
  strBlock: string;
  simpleObj: TSimpleObj;
begin
  simpleObj := TSimpleObj.Create;
  try
    FRlxRazorProcessor.AddToDictionary(SampleObjectName, SimpleObj, False);
    strBlock := '@if ' + SampleObjectName + '.' + SampleFieldName + ' {Hello}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('', ReturnValue);
    FRlxRazorProcessor.DataObjects.Remove(SampleObjectName);
  finally
    simpleObj.Free;
  end;
end;

procedure TestTRlxRazorProcessor.TestIfFalse;
var
  ReturnValue: string;
  strBlock: string;
  simpleObj: TSimpleObj;
begin
  simpleObj := TSimpleObj.Create;
  try
    simpleObj.ABool := False;
    FRlxRazorProcessor.AddToDictionary(SampleObjectName, SimpleObj, False);
    strBlock := '@if ' + SampleObjectName + '.' + SampleBoolFieldName + ' {Hello}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('', ReturnValue);
    FRlxRazorProcessor.DataObjects.Remove(SampleObjectName);
  finally
    simpleObj.Free;
  end;
end;

procedure TestTRlxRazorProcessor.TestIfNotEmptyString;
var
  ReturnValue: string;
  strBlock: string;
  simpleObj: TSimpleObj;
begin
  simpleObj := TSimpleObj.Create;
  try
    simpleObj.AField := 'something';
    FRlxRazorProcessor.AddToDictionary(SampleObjectName, SimpleObj, False);
    strBlock := '@if ' + SampleObjectName + '.' + SampleFieldName + ' {Hello}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('Hello', ReturnValue);
    FRlxRazorProcessor.DataObjects.Remove(SampleObjectName);
  finally
    simpleObj.Free;
  end;
end;

procedure TestTRlxRazorProcessor.TestIfTrue;
var
  ReturnValue: string;
  strBlock: string;
  simpleObj: TSimpleObj;
begin
  simpleObj := TSimpleObj.Create;
  try
    simpleObj.ABool := True;
    FRlxRazorProcessor.AddToDictionary(SampleObjectName, SimpleObj, False);
    strBlock := '@if ' + SampleObjectName + '.' + SampleBoolFieldName + ' {Hello}';
    ReturnValue := Trim(FRlxRazorProcessor.DoBlock(strBlock));
    CheckEquals ('Hello', ReturnValue);
    FRlxRazorProcessor.DataObjects.Remove(SampleObjectName);
  finally
    simpleObj.Free;
  end;
end;

procedure TestTRlxRazorProcessor.TestIsLogged;
var
  strBlock: string;
  ReturnValue: string;
begin
  FRlxRazorProcessor.UserLoggedIn := True;
  strBlock := 'Something @LoginRequired else';
  ReturnValue := FRlxRazorProcessor.DoBlock(strBlock);
  CheckEquals('Something  else', ReturnValue);
end;

procedure TestTRlxRazorProcessor.TestIsNotLogged;
begin
  FRlxRazorProcessor.UserLoggedIn := False;
  CheckException(RunLoginRequiredScript, ERlxLoginRequired);
end;

procedure TestTRlxRazorProcessor.TestPermission;
var
  strBlock: string;
  ReturnValue: string;
begin
  FRlxRazorProcessor.UserLoggedIn := True;
  FRlxRazorProcessor.UserRoles := 'Full, MyGroup, More';
  strBlock := 'Something @LoginRequired.MyGroup else';
  ReturnValue := FRlxRazorProcessor.DoBlock(strBlock);
  CheckEquals('Something  else', ReturnValue);
end;

procedure TestTRlxRazorProcessor.TestUnicode;
var
  ReturnValue: string;
  strBlock: UTF8String;
begin
  // generic text should remain the same after processing
  strBlock := 'âäçíïòýąÈËϘϟϡڏڧۋﻙﻵ';
  ReturnValue := FRlxRazorProcessor.DoBlock(strBlock, TEncoding.UTF8);
  CheckEquals (string(strBlock), ReturnValue);
end;

procedure TestTRlxRazorProcessor.TestValueEvent;
var
  ReturnValue: string;
  strBlock: string;
begin
  FRlxRazorProcessor.OnValue := TestValueHandler;
  strBlock := 'Anything @' + SampleObjectName + '.' + SampleFieldName;
  ReturnValue := FRlxRazorProcessor.DoBlock(strBlock);
  CheckEquals ('Anything ' + SampleFieldValue, ReturnValue);
end;

procedure TestTRlxRazorProcessor.TestValueFromDictionary;
var
  simpleObj: TSimpleObj;
  strBlock: string;
  ReturnValue: string;
begin
  // add non-owned object to processor
  simpleObj := TSimpleObj.Create;
  try
    simpleObj.AField := SampleFieldValue;
    FRlxRazorProcessor.AddToDictionary(SampleObjectName, SimpleObj, False);
    strBlock := 'Anything @' + SampleObjectName + '.' + SampleFieldName;
    ReturnValue := FRlxRazorProcessor.DoBlock(strBlock);
    CheckEquals ('Anything ' + SampleFieldValue, ReturnValue);
    FRlxRazorProcessor.DataObjects.Remove(SampleObjectName);
  finally
    simpleObj.Free;
  end;
end;

procedure TestTRlxRazorProcessor.TestValueHandler(Sender: TObject;
  const ObjectName, FieldName: string; var ReplaceText: string);
begin
  if SameText (ObjectName, SampleObjectName) and
      SameText (FieldName, SampleFieldName) then
    ReplaceText := SampleFieldValue;
end;

procedure TestTRlxRazorProcessor.TestWrongPermission;
begin
  FRlxRazorProcessor.UserLoggedIn := False;
  FRlxRazorProcessor.UserRoles := '';
  CheckException(RunLoginPermissionScript, ERlxLoginRequired);
end;

{ TSimpleObj }

procedure TSimpleObj.SetABool(const Value: Boolean);
begin
  FABool := Value;
end;

procedure TSimpleObj.SetAField(const Value: string);
begin
  FAField := Value;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTRlxRazorEngine.Suite);
  RegisterTest(TestTRlxRazorProcessor.Suite);
end.

