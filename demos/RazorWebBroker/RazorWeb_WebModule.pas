unit RazorWeb_WebModule;

interface

uses
  SysUtils, Classes, HTTPApp, RlxRazor, Generics.Collections, DB, DBClient;

type
  TWebModule13 = class(TWebModule)
    RlxRazorProcessor1: TRlxRazorProcessor;
    RlxRazorEngine1: TRlxRazorEngine;
    RlxRazorProcessor2: TRlxRazorProcessor;
    RlxRazorProcessor3: TRlxRazorProcessor;
    RlxRazorValueFromEvent: TRlxRazorProcessor;
    RlxRazorValueFromObject: TRlxRazorProcessor;
    ClientDataSet1: TClientDataSet;
    RlxRazorValueFromTable: TRlxRazorProcessor;
    RlxRazorIf: TRlxRazorProcessor;
    RlxRazorCompanyList: TRlxRazorProcessor;
    procedure WebModule13DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure RlxRazorProcessor1Lang(Sender: TObject; const FieldName: string;
      var ReplaceText: string);
    procedure RlxRazorEngine1Lang(Sender: TObject; const FieldName: string;
      var ReplaceText: string);
    procedure WebModule13WebActionItem3Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure RlxRazorValueFromEventValue(Sender: TObject; const ObjectName,
      FieldName: string; var ReplaceText: string);
    procedure WebModule13WebActionItem5Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule13waCompanyAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModule13waIfAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModule13waCompanyListAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule13;

implementation

{$R *.dfm}

type
  TSimpleObject = class
  private
    FName: string;
    FValue: Integer;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Integer);
    function GetValueBelowTen: Boolean;
  public
    constructor Create (aName: string; aValue: Integer);
    property Name: string read FName write SetName;
    property Value: Integer read FValue write SetValue;
    property ValueBelowTen: Boolean read GetValueBelowTen;
  end;

procedure TWebModule13.RlxRazorEngine1Lang(Sender: TObject;
  const FieldName: string; var ReplaceText: string);
begin
  if FieldName = 'hellow' then
    ReplaceText := 'Hello World from Engine';
end;

procedure TWebModule13.RlxRazorProcessor1Lang(Sender: TObject;
  const FieldName: string; var ReplaceText: string);
begin
  if FieldName = 'hellow' then
    ReplaceText := 'Hello World';
end;

procedure TWebModule13.RlxRazorValueFromEventValue(Sender: TObject;
  const ObjectName, FieldName: string; var ReplaceText: string);
begin
  if SameText (ObjectName, 'data') then
    ReplaceText := 'You requested ' + FieldName;
end;

procedure TWebModule13.WebModule13DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Found: Boolean;
begin
  Response.Content := RlxRazorEngine1.ProcessRequest (Request, Found);
  Response.ContentType := 'text/html';
  if not Found then
  begin
    Response.StatusCode := 404;
    Response.ReasonString := 'File not found';
  end;
end;

procedure TWebModule13.WebModule13waCompanyAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  ClientDataSet1.Open;
  RlxRazorValueFromTable.AddToDictionary(
    'dataset', ClientDataSet1, False); // do not destroy
  Response.Content := RlxRazorValueFromTable.Content;
  ClientDataSet1.Close;
  Handled := True;
end;

procedure TWebModule13.WebModule13waCompanyListAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  ClientDataSet1.Open;
  RlxRazorCompanyList.AddToDictionary(
    'dataset', ClientDataSet1, False); // do not destroy
  Response.Content := RlxRazorCompanyList.Content;
  ClientDataSet1.Close;
  Handled := True;
end;

procedure TWebModule13.WebModule13waIfAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  RlxRazorIf.AddToDictionary('obj1',
    TSimpleObject.Create ('joe', 55));
  RlxRazorIf.AddToDictionary('obj2',
    TSimpleObject.Create ('marc', 9));
  Response.Content := RlxRazorIf.Content;
  Handled := True;
end;

procedure TWebModule13.WebModule13WebActionItem3Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := RlxRazorProcessor3.Content;
  Handled := True;
end;

procedure TWebModule13.WebModule13WebActionItem5Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if not RlxRazorValueFromObject.InDictionary ('data') then
    RlxRazorValueFromObject.AddToDictionary('data',
      TSimpleObject.Create ('joe', 55));
  Response.Content := RlxRazorValueFromObject.Content;
  Handled := True;
end;

procedure TWebModule13.WebModuleCreate(Sender: TObject);
begin
  RlxRazorProcessor3.AddToDictionary ('page',
    TSimpleObject.Create ('one', random (100)));
end;

{ TSimpleObject }

constructor TSimpleObject.Create(aName: string; aValue: Integer);
begin
  inherited Create;
  fName := aName;
  fValue := aValue;
end;

function TSimpleObject.GetValueBelowTen: Boolean;
begin
  Result := FValue < 10;
end;

procedure TSimpleObject.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TSimpleObject.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

end.
