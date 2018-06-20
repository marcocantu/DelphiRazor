unit RlxRazor;

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
///   Francesco Fontana, Marco Cantu, Marco Mottadelli          ///
///                                                             ///
///////////////////////////////////////////////////////////////////

interface

uses
  Classes, HTTPProd, DB, HTTPApp, SysUtils, CopyPrsr, Generics.Collections;

type
  ERlxLoginRequired = class (Exception);
  TRlxRazorProcessor = class; // forward declaration
  TRlxRazorEngine = class;

  TRazorValueEvent = procedure(Sender: TObject; const ObjectName: string;
    const FieldName: string; var ReplaceText: string) of object;

  TRazorLanguageEvent = procedure(Sender: TObject; const FieldName: string;
    var ReplaceText: string) of object;

  TRazorScaffoldingEvent = procedure(Sender: TObject; const qualifClassName: string;
    var ReplaceText: string) of object;

  TRazorWarnEvent = procedure(Sender: TObject; const strWarning: string)
    of object;

  // uses a record to apss this data strucvutre (used for path initialization, to avoid complex event handler and allow
  // udpates without breaking the event handler interface
  TRazorExecData = record
    PathInfo: string;
    PathParam: string;
    razorProc: TRlxRazorProcessor;
  end;

  TRazorObjectOnPath = procedure(Sender: TObject; ExecData: TRazorExecData) of object;

  TInitFunction = reference to function (): TObject;

  TRazDictItemKind = (rdikObject, rdikDataset);

  TRazDictItem = class
  private
    FTheObject: TObject;
    FKind: TRazDictItemKind;
    FOwned: Boolean;
    FInitProcedure: TInitFunction;
    procedure SetKind(const Value: TRazDictItemKind);
    procedure SetOwned(const Value: Boolean);
    procedure SetTheObject(const Value: TObject);
    procedure SetInitProcedure(const Value: TInitFunction);
  public
    property InitProcedure: TInitFunction read FInitProcedure write SetInitProcedure;
    property TheObject: TObject read FTheObject write SetTheObject;
    property Kind: TRazDictItemKind read FKind write SetKind;
    property Owned: Boolean read FOwned write SetOwned;
  end;

  TRazorDictionary = TDictionary <string, TRazDictItem>;

  // alternative behaviors in case of duplicates
  TDictionaryDuplicates = (ddIgnore, ddReplace, ddError);

  TRazLoopVar = class
  private
    FLoopObject: TObject;
    // FKind: TRazDictItemKind;
    FCurrentObj: TObject;
    FRazProc: TRlxRazorProcessor;
    FLoopObjTokenStr: string;
    FLoopObjTokenAfterDotStr: string;
    FLoopVarTokenStr: string;
    FCurrPos: Integer;
    procedure SetLoopObject(const Value: TObject);
    procedure SetCurrentObj(const Value: TObject);
  public
    constructor Create (aRazProc: TRlxRazorProcessor; const aLoopObjToken, aLoopVarToken, aLoopObjAfterDotToken: string);
    function InitLoopObjects: Boolean; // init counts, return LoopObject
    function NextLoopObject: Boolean;  // return NextLoopObject
    property LoopObjTokenStr: string read FLoopObjTokenStr;
    property LoopObjTokenAfterDotStr: string read FLoopObjTokenAfterDotStr;
    property LoopVarTokenStr: string read FLoopVarTokenStr;
    property LoopObject: TObject read FLoopObject write SetLoopObject;
    property CurrentObj: TObject read FCurrentObj write SetCurrentObj;
  end;

  TRazorLoopVars = TObjectDictionary <string, TRazLoopVar>;

  // request information
  TPageInfo = class
  private
    FItem: string;
    FAction: string;
    FPage: string;
    FReferer: string;
    FBrowser: string;
    FAddress: string;
    FUserLogged: Boolean;
    FUserRole: string;
    procedure SetAction(const Value: string);
    procedure SetItem(const Value: string);
    procedure SetPage(const Value: string);
    procedure SetReferer(const Value: string);
    procedure SetBrowser(const Value: string);
    procedure SetAddress(const Value: string);
    procedure SetUserRole(const Value: string);
    procedure SetUserLogged(const Value: Boolean);
    function GetUserNotLogged: Boolean;
    procedure SetUserNotLogged(const Value: Boolean);
  public
    property Action: string read FAction write SetAction;
    property Page: string read FPage write SetPage;
    property Item: string read FItem write SetItem;
    property Referer: string read FReferer write SetReferer;
    property Browser: string read FBrowser write SetBrowser;
    property Address: string read FAddress write SetAddress;
    // user information
    property UserLogged: Boolean read FUserLogged write SetUserLogged;
    property UserNotLogged: Boolean read GetUserNotLogged write SetUserNotLogged;
    property UserRole: string read FUserRole write SetUserRole;
  end;

  TRazorLogEvent = procedure(Sender: TObject; pageInfo: TPageInfo) of object;

  TRazorPathInitEvent = procedure (Sender: TObject; ExecData: TRazorExecData) of object;

  TRazorPathInitItem = class (TCollectionItem)
  private
    FPath: string;
    FOnPathInit: TRazorPathInitEvent;
    procedure SetOnPathInit(const Value: TRazorPathInitEvent);
    procedure SetPath(const Value: string);
  public
    procedure DoInit (execData: TRazorExecData);
  published
    property Path: string read FPath write SetPath;
    property OnPathInit: TRazorPathInitEvent read FOnPathInit write SetOnPathInit;
  end;

  TRazorPathInitCollection = class (TCollection)
  private
    function GetItem(Index: Integer): TRazorPathInitItem;
    procedure SetItem(Index: Integer; const Value: TRazorPathInitItem);
  public
    constructor Create;
    function Add: TRazorPathInitItem;
    function AddPathItem (const Path: string; InitMethod: TRazorPathInitEvent): TRazorPathInitItem;
    function TryGetPathItem (const Path: string; var pathItem: TRazorPathInitItem): Boolean;
    property Items [Index: Integer]: TRazorPathInitItem
      read GetItem write SetItem; default;
  end;

  /// <summary>
  /// The Razor Engine class exposes customizations parameters
  /// and event handlers shared by all instances of RazorProcessor
  /// connected to it. It is not compulsory, through.
  /// </summary>
  TRlxRazorEngine = class(TComponent)
  private
    fRazorValueEvent: TRazorValueEvent;
    FTemplatesFolder: string;
    FFilesFolder: string;
    FOnLang: TRazorLanguageEvent;
    FRazorWarnEvent: TRazorWarnEvent;
    FOnObjectForPath: TRazorObjectOnPath;
    FHomePage: string;
    FOnScaffolding: TRazorScaffoldingEvent;
    FDataObjects: TRazorDictionary;
    FDictionaryDuplicates: TDictionaryDuplicates;
    FErrorPage: string;
    FOnPageLog: TRazorLogEvent;
    FOnPageError: TRazorLogEvent;
    FAliases: TStrings;
    FPathInits: TRazorPathInitCollection;
    FBasePath: string;
    procedure SetFilesFolder(const Value: string);
    procedure SetTemplatesFolder(const Value: string);
    procedure SetRazorValueEvent(const Value: TRazorValueEvent);
    procedure SetOnLang(const Value: TRazorLanguageEvent);
    procedure SetRazorWarnEvent(const Value: TRazorWarnEvent);
    procedure ConnectObjectForPath (execData: TRazorExecData);
    procedure SetOnObjectForPath(const Value: TRazorObjectOnPath);
    procedure SetHomePage(const Value: string);
    procedure SetOnScaffolding(const Value: TRazorScaffoldingEvent);
    procedure ProcessPath (strPath: string; sList: TStringList);
    procedure StripPath(sPath: TStringList; const PathToStrip: string);
    procedure SetDictionaryDuplicates(const Value: TDictionaryDuplicates);
    procedure SetErrorPage(const Value: string);
    procedure SetAliases(const Value: TStrings);
    procedure SetPathInits(const Value: TRazorPathInitCollection);
  public
    constructor Create (Owner: TComponent); override;
    function GetTemplatesFolder: string;
    // if defined, if not the files folder
    function GetOtherValue(const ObjectName, FieldName: string): string;  virtual;
    function GetLang(const FieldName: string): string;  virtual;
    function GetScaffolding (const strDottedClass: string): string; virtual;
    procedure AddWarning(const strWarning: string);
    // operations
    function ProcessRequest(Request: TWebRequest;
      var Found: Boolean; LoggedUser: Boolean = False;
      LanguageID: Integer = 1; inFolder: string = ''; userRoles: string = ''): string;
    destructor Destroy; override;
    class function GetLangFormats (language_id: Integer): TFormatSettings;
    procedure AddToDictionary (aName: string; theObject: TObject; Owned: Boolean = True); overload;
    function InDictionary (aName: string): Boolean;
    property DataObjects: TRazorDictionary read FDataObjects;
  protected
    procedure DictionaryItemNotification (Sender: TObject;
      const Item: TRazDictItem; Action: TCollectionNotification);
  published
    // properties
    property BasePath: string read FBasePath write FBasePath;
    property PathInits: TRazorPathInitCollection read FPathInits write SetPathInits;
    property FilesFolder: string read FFilesFolder write SetFilesFolder;
    property TemplatesFolder: string read FTemplatesFolder
      write SetTemplatesFolder;
    property HomePage: string read FHomePage write SetHomePage;
    property ErrorPage: string read FErrorPage write SetErrorPage;
    property Aliases: TStrings read FAliases write SetAliases;
    // events
    property OnValue: TRazorValueEvent read fRazorValueEvent
      write SetRazorValueEvent;
    property OnWarn: TRazorWarnEvent read FRazorWarnEvent
      write SetRazorWarnEvent;
    property OnLang: TRazorLanguageEvent read FOnLang write SetOnLang;
    property OnObjectForPath: TRazorObjectOnPath
      read FOnObjectForPath write SetOnObjectForPath;
    property OnScaffolding: TRazorScaffoldingEvent read FOnScaffolding write SetOnScaffolding;
    property OnPageLog: TRazorLogEvent read FOnPageLog write FOnPageLog;
    property OnPageError: TRazorLogEvent read FOnPageError write FOnPageError;
    property DictionaryDuplicates: TDictionaryDuplicates
      read FDictionaryDuplicates write SetDictionaryDuplicates
      default ddIgnore;
  end;

  /// <summary>
  /// The Razor Processor class is used to process an individual file
  /// (generally with HTML extension) and its associated template if any
  /// </summary>
  TRlxRazorProcessor = class(TCustomContentProducer)
  private
    fRazorValueEvent: TRazorValueEvent;
    fInputFilename: string;
    FRequest: TWebRequest;
    FRazorEngine: TRlxRazorEngine;
    FOnLang: TRazorLanguageEvent;
    FRazorWarnEvent: TRazorWarnEvent;
    FDataObjects: TRazorDictionary;
    FLoopVars: TRazorLoopVars;
    FAddedPages: TStringList;
    // temps for loops old version
    aLoopList: TList <TObject>;
    aLoopListItem: Integer;
    aLoopDataSet: TDataSet;
    FUserLoggedIn: Boolean;
    FLanguageId: Integer;
    FOnScaffolding: TRazorScaffoldingEvent;
    FDictionaryDuplicates: TDictionaryDuplicates;
    FUserRoles: string;

    function GetDictionaryValue(item: TRazDictItem; const FieldName: string): string;
    function GetQueryField(const FieldName: string): string;
    procedure SetRequest(const Value: TWebRequest);
    procedure SetRazorEngine(const Value: TRlxRazorEngine);
    function GetTemplatesFolder: string;
    procedure SetRazorValueEvent(const Value: TRazorValueEvent);
    function ProcessDottedValue(TokenStr: string; Parser: TCopyParser;
      Encoding: TEncoding): string;
    function EvalBooleanDottedValue(TokenStr: string; Parser: TCopyParser;
      Encoding: TEncoding): Boolean;
    procedure SetOnLang(const Value: TRazorLanguageEvent);
    procedure SetRazorWarnEvent(const Value: TRazorWarnEvent);
    procedure SetUserLoggedIn(const Value: Boolean);
    procedure SetLanguageId(const Value: Integer);
    function GetObjectProperty(anObj: TObject; const propName: string;
      var propValue: string): Boolean;
    function GetFormatSettings: TFormatSettings;
    function GetOtherValue(const ObjectName, FieldName: string): string;
    function GetEntireDottedValue (Parser: TCopyParser; Encoding: TEncoding): string;
    procedure SetOnScaffolding(const Value: TRazorScaffoldingEvent);
    procedure SetDictionaryDuplicates(const Value: TDictionaryDuplicates);
    procedure SetUserRoles(const Value: string);
    function ProcessLoopDottedValue(LoopVar: TRazLoopVar; Parser: TCopyParser; Encoding: TEncoding): string;
    function GetSubObject(anObj: TObject; const propName: string; var retObj: TObject): Boolean;
    function GetLoopOrDictionaryObject(const LoopObjTokenStr, LoopVarTokenStr: string): TObject;
    function GetCurrenctObject(const LoopVarTokenStr: string): TObject;
    function EvalBooleanLoopDottedValue(LoopVar: TRazLoopVar; Parser: TCopyParser;
      Encoding: TEncoding): Boolean;
    function EvalBooleanObjectProperty(anObj: TObject; const propName: string;
      var Value: Boolean): Boolean;
  protected
    function RazorContentFromStream(AStream: TStream): string;
    function RazorContentFromFile: string;
    function RazorContentFromTemplate(const filename: string): string;
    procedure AddWarning(const strWarning: string); virtual;
    function GetLang(const FieldName: string): string; virtual;
    function GetScaffoding (const strDottedClass: string): string; virtual;
    function LoopBlock(const strBlock, strEnumeration: string; Encoding: TEncoding): string;
    function LoopNestedBlock(const strBlock: string; LoopVar: TRazLoopVar; Encoding: TEncoding): string;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DictionaryItemNotification (Sender: TObject;
      const Item: TRazDictItem; Action: TCollectionNotification);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function Content: string; override;
    function DoBlock(const strBlock: RawByteString; Encoding: TEncoding = nil): string;
    procedure AddToDictionary (aName: string; theObject: TObject; Owned: Boolean = True); overload;
    procedure AddToDictionary (aName: string; aInitProc: TInitFunction); overload;
    function InDictionary (aName: string): Boolean;
    property Request: TWebRequest read FRequest write SetRequest;
    property DataObjects: TRazorDictionary read FDataObjects;
  published
    // properties
    property InputFilename: string read fInputFilename write fInputFilename;
    property RazorEngine: TRlxRazorEngine read FRazorEngine
      write SetRazorEngine;
    // events
    property OnValue: TRazorValueEvent read fRazorValueEvent
      write SetRazorValueEvent;
    property OnWarn: TRazorWarnEvent read FRazorWarnEvent
      write SetRazorWarnEvent;
    property OnLang: TRazorLanguageEvent read FOnLang write SetOnLang;
    property UserLoggedIn: Boolean read FUserLoggedIn write SetUserLoggedIn;
    property UserRoles: string read FUserRoles write SetUserRoles;
    property LanguageId: Integer read FLanguageId write SetLanguageId;
    property OnScaffolding: TRazorScaffoldingEvent
      read FOnScaffolding write SetOnScaffolding;
    property DictionaryDuplicates: TDictionaryDuplicates
      read FDictionaryDuplicates write SetDictionaryDuplicates
      default ddIgnore;
  end;

  ERazorException = class(Exception);

function DoubleAtSymbol (const strInput: string): string;
function RlxCopyLeft (S: String; n: integer): String;
function RlxCopyRight (S: String; n: integer): String;
function RlxExtractString (S: string; Po: integer; Ch: char; toEnd: boolean): string;

implementation

uses
  RTTI, TypInfo, StrUtils, RlxLocales;

const
  strFileNotFound = 'filenotfound';

// from SysUtils, should be fine also for NextGen!
function BytesOf(const Val: RawByteString): TBytes;
var
  Len: Integer;
begin
  Len := Length(Val);
  SetLength(Result, Len);
  Move(Val[1], Result[0], Len);
end;

{ TRlxRazorProcessor }

function TRlxRazorProcessor.Content: string;
begin
  Result := RazorContentFromFile;
end;

procedure TRlxRazorProcessor.AddToDictionary(aName: string; theObject: TObject;
  Owned: Boolean);
var
  item: TRazDictItem;
begin
  if FDataObjects.ContainsKey(aName) then
    case FDictionaryDuplicates of
      ddIgnore:
      begin
        if Owned then
          theObject.Free;
        Exit;
      end;
      ddReplace: FDataObjects.Remove(aName);
      ddError: ; // let it go, will raise an error!
    end;

  item := TRazDictItem.Create;
  if theObject is TDataSet then
    item.FKind := rdikDataset
  else
    item.FKind := rdikObject;
  item.TheObject := theObject;
  item.Owned := Owned;
  FDataObjects.Add (aName, item);
end;

procedure TRlxRazorProcessor.AddWarning(const strWarning: string);
begin
  if Assigned(FRazorWarnEvent) then
  begin
    FRazorWarnEvent(self, strWarning);
  end
  else if Assigned(FRazorEngine) then
  begin
    FRazorEngine.AddWarning(strWarning);
  end;
end;

function TRlxRazorProcessor.ProcessDottedValue(TokenStr: string;
  Parser: TCopyParser; Encoding: TEncoding): string;
var
  TokenAfterDot: string;
  strResult: string;
  item: TRazDictItem;
begin
  Result := '';
  // if followed by ., read the next element
  if Parser.Token = '.' then
  begin
    Parser.SkipToken(True);
    TokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));
  end;
  // depending on the main symbol: special processing
  if TokenStr = 'lang' then
    Result := GetLang(TokenAfterDot)
  else if TokenStr = 'query' then
    Result := GetQueryField(TokenAfterDot)
  else if (TokenStr = 'loop') then
  begin
    if Assigned (aLoopDataSet) then
      Result := aLoopDataSet.FieldByName(TokenAfterDot).AsString;
    if Assigned (aLoopList) then
    begin
      if GetObjectProperty (aLoopList[aLoopListItem], TokenAfterDot, strResult) then
        Result := strResult;
    end;
  end
  else
  begin
    // generic processing
    if DataObjects.TryGetValue (TokenStr, item) then
      Result := GetDictionaryValue(item, TokenAfterDot)
    else if Assigned (RazorEngine) and RazorEngine.DataObjects.TryGetValue (TokenStr, item) then
      Result := GetDictionaryValue(item, TokenAfterDot)
    else
      Result := GetOtherValue(TokenStr, TokenAfterDot);
  end;

  // reparse for double processing
  if Pos ('@', Result) > 0 then
  begin
    Result := DoBlock (RawByteString(Result), Encoding);
  end;
end;

function TRlxRazorProcessor.ProcessLoopDottedValue(LoopVar: TRazLoopVar;
  Parser: TCopyParser; Encoding: TEncoding): string;
var
  TokenAfterDot, strResult: string;
begin
  Result := '';

  // must followed by . and the element name
  if Parser.Token = '.' then
  begin
    Parser.SkipToken(True);
    TokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));

    if LoopVar.LoopObject is TDataSet then
      Result := (LoopVar.LoopObject as TDataSet).
        FieldByName(TokenAfterDot).AsString
    else if GetObjectProperty (LoopVar.CurrentObj, TokenAfterDot, strResult) then
      Result := strResult
    else
      AddWarning ('Missing value ' + TokenAfterDot + ' for loop variable ' + LoopVar.LoopVarTokenStr);
  end
  else
    AddWarning ('Missing . after loop variable ' + LoopVar.LoopVarTokenStr);
end;

function TRlxRazorProcessor.EvalBooleanDottedValue(TokenStr: string;
  Parser: TCopyParser; Encoding: TEncoding): Boolean;
var
  TokenAfterDot: string;
  bResult: Boolean;
  item: TRazDictItem;
  //dictObj: TObject;
begin
  Result := False;
  // if followed by ., read the next element
  if Parser.Token = '.' then
  begin
    Parser.SkipToken(True);
    TokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));
  end;
  if TokenStr = 'query' then
    Result := GetQueryField(TokenAfterDot) <> ''
  else if (TokenStr = 'loop') then
  begin
    if Assigned (aLoopDataSet) then
      Result := aLoopDataSet.FieldByName(TokenAfterDot).AsString <> '';  // TODO consider other data types!!!!!!
    if Assigned (aLoopList) then
    begin
      if EvalBooleanObjectProperty (aLoopList[aLoopListItem], TokenAfterDot, bResult) then
        Result := bResult;
    end;
  end
  else
  begin
    // generic processing
    if DataObjects.TryGetValue (TokenStr, item) then
    begin
      if EvalBooleanObjectProperty (item.TheObject, TokenAfterDot, bResult) then
        Result := bResult;
    end
    else if Assigned (RazorEngine) and RazorEngine.DataObjects.TryGetValue (TokenStr, item) then
    begin
       if EvalBooleanObjectProperty (item.TheObject, TokenAfterDot, bResult) then
        Result := bResult
    end
    else
      // TODO add an eval method for booleans?
      Result := GetOtherValue(TokenStr, TokenAfterDot) <> '';
  end;
end;

function TRlxRazorProcessor.EvalBooleanLoopDottedValue(LoopVar: TRazLoopVar;
  Parser: TCopyParser; Encoding: TEncoding): Boolean;
var
  TokenAfterDot: string;
  bResult: Boolean;
begin
  Result := False;
  // must followed by . and the element name
  if Parser.Token = '.' then
  begin
    Parser.SkipToken(True);
    TokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));
    if EvalBooleanObjectProperty (LoopVar.CurrentObj, TokenAfterDot, bResult) then
      Result := bResult
    else
      AddWarning ('Missing value ' + TokenAfterDot + ' for loop variable ' + LoopVar.LoopVarTokenStr);
  end
  else
    AddWarning ('Missing . after loop variable ' + LoopVar.LoopVarTokenStr);
end;


constructor TRlxRazorProcessor.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FDataObjects := TRazorDictionary.Create;
  FDataObjects.OnValueNotify := DictionaryItemNotification;
  FDictionaryDuplicates := ddIgnore;
  FLanguageID := 0; // use a global default value instead?
  FLoopVars := TRazorLoopVars.Create ([doOwnsValues]);
  FAddedPages := TStringList.Create;
end;

destructor TRlxRazorProcessor.Destroy;
begin
  FDataObjects.Free;
  FLoopVars.Free;
  FAddedPages.Free;
  inherited;
end;

procedure TRlxRazorProcessor.DictionaryItemNotification(Sender: TObject;
  const Item: TRazDictItem; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
  begin
    if Item.Owned then
    begin
{$IFNDEF NEXTGEN}
      Item.TheObject.Free;
{$ENDIF}
      Item.TheObject := nil;
    end;
{$IFNDEF NEXTGEN}
    Item.Free;
{$ENDIF}
  end;
end;

function TRlxRazorProcessor.DoBlock(const strBlock: RawByteString; Encoding: TEncoding): string;
var
  strStream: TStringStream;
begin
  Result := '';

  if not Assigned (Encoding) then
    Encoding := TEncoding.Default;

  strStream := TStringStream.Create(string(strBlock), Encoding);
  try
    Result := RazorContentFromStream(strStream);
  finally
    strStream.Free;
  end;
end;

function TRlxRazorProcessor.GetLoopOrDictionaryObject(
  const LoopObjTokenStr, LoopVarTokenStr: string): TObject;
var
  bFound: Boolean;
  loopVar: TRazLoopVar;
  dictVar: TRazDictItem;
begin
  Result := nil;

  // search loop variables first
  bFound := FLoopVars.TryGetValue (LoopObjTokenStr, loopVar);
  if bFound then
    if Assigned(loopVar.LoopObject) then
      Exit(loopVar.LoopObject);

  // now search the local dictionary
  bFound := DataObjects.TryGetValue (LoopObjTokenStr, dictVar);
  if bFound then
    if Assigned(dictVar.TheObject) then
      Exit (dictVar.TheObject); // the current element of the loop

  // search the engine (engine dictionary support seems incomplete)
  if Assigned (RazorEngine) then
  begin
    bFound := RazorEngine.DataObjects.TryGetValue (LoopObjTokenStr, dictVar);
    if bFound then
      if Assigned(dictVar.TheObject) then
        Exit (dictVar.TheObject); // the current element of the loop
  end;
end;

function TRlxRazorProcessor.GetCurrenctObject(
  const LoopVarTokenStr: string): TObject;
var
  loopVar: TRazLoopVar;
  bFound: Boolean;
begin
  Result := nil;

  bFound := FLoopVars.TryGetValue (LoopVarTokenStr, loopVar);
  if bFound then
    if Assigned(loopVar.CurrentObj) then
      Exit(loopVar.CurrentObj);
end;

function TRlxRazorProcessor.GetDictionaryValue(item: TRazDictItem;
  const FieldName: string): string;
var
  aDbField: TField;
  strResult: string;
begin
  strResult := '';
  // check if the object has been initialized
  if not Assigned (item.TheObject) then
  begin
    if Assigned (item.InitProcedure) then
    begin
      item.TheObject := item.InitProcedure();
      if item.theObject is TDataSet then
        item.FKind := rdikDataset
      else
        item.FKind := rdikObject;
    end;
  end;
  case item.Kind of
    rdikObject:
    begin
      if Assigned(item.TheObject) then
      begin
        if not GetObjectProperty (item.TheObject, FieldName, strResult) then
        begin
          AddWarning(Result);
          Result := fieldname;
        end
        else
          Result := strResult;
      end;
    end;
    rdikDataset:
    begin
      if Assigned(item.TheObject) and (Item.TheObject is TDataSet) then
      begin
        aDbField := TDataSet (Item.TheObject).FieldByName (FieldName);
        if Assigned (aDbField) then
          Result := aDbField.AsString
        else
        begin
          AddWarning('Missing field name ' + FieldName);
          Result := fieldname;
        end;
      end;
    end;
  end;
end;

function TRlxRazorProcessor.GetEntireDottedValue (Parser: TCopyParser; Encoding: TEncoding): string;
var
  TokenAfterDot: string;
begin
  Result := Encoding.GetString(BytesOf(Parser.TokenString));
  Parser.SkipToken(True);
  // if followed by ., read the next element
  if Parser.Token = '.' then
  begin
    Parser.SkipToken(True);
    TokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));
    Result := Result + '.' + TokenAfterDot;
    Parser.SkipToken(True);
  end;
end;

function TRlxRazorProcessor.GetFormatSettings: TFormatSettings;
begin
  if Assigned (FRazorEngine) then
    Result := FRazorEngine.GetLangFormats (LanguageId)
  else
    Result := FormatSettings;
end;

function TRlxRazorProcessor.GetLang(const FieldName: string): string;
begin
  Result := FieldName; // default
  if Assigned(FOnLang) then
    FOnLang(self, FieldName, Result)
  else if Assigned(FRazorEngine) then
  begin
    Result := FRazorEngine.GetLang(FieldName);
  end;
end;

function TRlxRazorProcessor.GetOtherValue(const ObjectName,
  FieldName: string): string;
begin
  Result := ObjectName + '.' + FieldName; // return the input
  if Assigned(fRazorValueEvent) then
    fRazorValueEvent(self, ObjectName, FieldName, Result)
  else if Assigned(FRazorEngine) then
  begin
    Result := FRazorEngine.GetOtherValue(ObjectName, FieldName);
  end;
end;

function DoubleAtSymbol (const strInput: string): string;
begin
  Result := strInput;
  // double any @, as following code will remove them
  if Pos ('@', Result) > 0 then
    Result := StringReplace (Result, '@', '@@', [rfReplaceAll]);
end;

function RlxCopyLeft (S: String; n: integer): String;
begin
  Result := Copy (S, 1, n);
end;

function RlxCopyRight(S:String; n:integer): String;
begin
  IF Length(S)>n then Result:=Copy(S,Length(S)-n+1,n)
                 else Result:=S;
end;

function RlxExtractString (S: string; Po: integer; Ch: char; toEnd: boolean): string;
var
  I, N: integer;
  StrO: string;
begin
  StrO := '';
  N := 0;

  for I:=1 to length(S) do
  begin
    if (((N = Po-1) and not ToEnd) or ((N >= Po-1) and ToEnd)) and ((S[I] <> Ch) or ToEnd) then
      StrO := StrO + S[I];
    if S[I] = ch then
      N := N + 1;
    if (N > Po-1) and not ToEnd then
      Break;
  end;

  Result := StrO;
end;

function TRlxRazorProcessor.GetQueryField(const FieldName: string): string;
begin
  Result := '';
  if Assigned(Request) then
  begin
    Result := Request.QueryFields.Values[FieldName];
    if Result = '' then
      AddWarning('Missing query.' + FieldName);
  end
  else if Assigned(Dispatcher) and Assigned(Dispatcher.Request) then
  begin
    Result := Dispatcher.Request.QueryFields.Values[FieldName];
    if Result = '' then
      AddWarning('Missing query.' + FieldName);
  end
  else
    AddWarning('Missing HTTP request object');
  // there might be an @ in a input field, need to double it or it will be processed
  Result := DoubleAtSymbol (Result);
end;

function TRlxRazorProcessor.GetScaffoding(const strDottedClass: string): string;
begin
  Result := strDottedClass; // default
  if Assigned(FOnScaffolding) then
    FOnScaffolding(self, strDottedClass, Result)
  else if Assigned(FRazorEngine) then
  begin
    Result := FRazorEngine.GetScaffolding(strDottedClass);
  end;
end;

function TRlxRazorProcessor.GetTemplatesFolder: string;
begin
  if Assigned(FRazorEngine) then
    Result := FRazorEngine.GetTemplatesFolder
  else
    Result := ExtractFilePath(fInputFilename);
end;

function TRlxRazorProcessor.InDictionary(aName: string): Boolean;
begin
  Result := FDataObjects.ContainsKey (aName);
end;

function TRlxRazorProcessor.LoopBlock(const strBlock, strEnumeration: string; Encoding: TEncoding): string;
var
  strStream: TStringStream;
  item: TRazDictItem;
  I: Integer;
begin
  Result := '';
  strStream := TStringStream.Create(strBlock);
  try
    if not DataObjects.TryGetValue (strEnumeration, item) then
      if Assigned (RazorEngine) then
        RazorEngine.DataObjects.TryGetValue (strEnumeration, item);

    if not Assigned (item) then
    begin
     AddWarning('Foreach enumeration object not found for ' + strEnumeration);
     Exit;
    end;

    if Assigned (item) and (item.TheObject is TList <TObject>) then
    begin
      aLoopList := item.TheObject as TList <TObject>;
      for I := 0 to aLoopList.Count - 1 do
      begin
        aLoopListItem := I;
        strStream.Position := 0; // reset
        Result := Result + RazorContentFromStream(strStream);
      end;
      // do loop
    end
    else if Assigned (item) and (item.TheObject is TDataSet) then
    begin
      aLoopDataSet := item.TheObject as TDataSet;
      // do loop
      aLoopDataSet.Open;
      aLoopDataSet.First;
      while not(aLoopDataSet.EOF) do
      begin
        strStream.Position := 0; // reset
        Result := Result + RazorContentFromStream(strStream);
        aLoopDataSet.Next;
      end;
    end;
  finally
    strStream.Free;
  end;
end;

function TRlxRazorProcessor.LoopNestedBlock(const strBlock: string;
  LoopVar: TRazLoopVar; Encoding: TEncoding): string;
var
  strStream: TStringStream;
begin
  Result := '';
  strStream := TStringStream.Create(strBlock, Encoding);
  try
    if LoopVar.InitLoopObjects then
    begin
      while LoopVar.NextLoopObject do
      begin
        strStream.Position := 0; // reset
        Result := Result + RazorContentFromStream(strStream);
      end;
    end;
  finally
    strStream.Free;
  end;
end;

procedure TRlxRazorProcessor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  // if the engine is begin destroyed, nil the reference
  if AComponent = FRazorEngine then
    FRazorEngine := nil;
end;

function TRlxRazorProcessor.RazorContentFromFile: string;
var
  fileStream: TFileStream;
begin
  Result := '';
  // internal variables cleanup
  FAddedPages.Clear;
  fileStream := TFileStream.Create(fInputFilename,
    fmOpenRead or fmShareDenyWrite);
  try
    Result := RazorContentFromStream(fileStream);
  finally
    fileStream.Free;
  end;
end;

function TRlxRazorProcessor.RazorContentFromStream(AStream: TStream): string;
var
  Parser: TCopyParser;
  ParsedTemplate: string;
  ParsedExtraHeader: string;
  AddedPageContent: string;
  TokenAfterDot: string;
  OutStream: TStringStream;
  TokenStr, followToken, blockAsString: string;
  ifValue: Boolean;
  LoopVar: TRazLoopVar;
  Encoding: TEncoding;
  SignatureSize: Integer;
  AddedPagesText: string;
  I: Integer;

  // returns loop object token
  function EvaluateParenthesis: TRazLoopVar;
  var
    VarToken, LoopVarToken, LoopCurrentObjToken, LoopObjTokenAfterDot, InToken, ParCloseToken: string;
  begin
    Parser.SkipToken(True);  // Skip (

    VarToken := Encoding.GetString(BytesOf(Parser.TokenString));
    if VarToken <> 'var' then
      AddWarning('missing var after (');
    Parser.SkipToken(True);  // Skip var

    LoopVarToken := Encoding.GetString(BytesOf(Parser.TokenString));
    Parser.SkipToken(True);  // skip loop object

    InToken := Encoding.GetString(BytesOf(Parser.TokenString));
    if InToken <> 'in' then
      AddWarning('missing in after loop object');
    Parser.SkipToken(True);  // Skip in

    LoopCurrentObjToken := Encoding.GetString(BytesOf(Parser.TokenString));
    Parser.SkipToken(True);  // Skip list object
    ParCloseToken := Encoding.GetString(BytesOf(Parser.TokenString));

    // complex objects could be good, but keep only 2 levels for now.
//    while ParCloseToken='.' do
//    begin
//      Parser.SkipToken(True);  // Skip dot
//      LoopObjToken := LoopObjToken + '.' + Encoding.GetString(BytesOf(Parser.TokenString));
//      Parser.SkipToken(True);  // Skip object property
//      ParCloseToken := Encoding.GetString(BytesOf(Parser.TokenString));
//    end;

    if ParCloseToken='.' then
    begin
      Parser.SkipToken(True);  // Skip dot
      LoopObjTokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));
      Parser.SkipToken(True);  // Skip object property
      ParCloseToken := Encoding.GetString(BytesOf(Parser.TokenString));
    end;

    if ParCloseToken <> ')' then AddWarning('missing ) after list object');
    Parser.SkipToken(True); // Skip )

    // add loop variable to current list and return it

    // Verifica se esiste nel dizionario l'oggetto di loop
    if FLoopVars.ContainsKey(LoopVarToken) then
    begin
      Result := FLoopVars.Items[LoopVarToken];
    end
    else
    begin
      Result := TRazLoopVar.Create (Self, LoopCurrentObjToken, LoopVarToken, LoopObjTokenAfterDot);
      FLoopVars.Add(LoopVarToken, Result);
    end;
  end;

  // return block within braces
  function FindMatchingClosingBrace: string;
  var
    ParOpenCounter: Integer;
    SavPos, SavSize: Int64;
  begin
    ParOpenCounter := 0;
    SavPos := OutStream.Position;
    SavSize := OutStream.Size;

    repeat
      followToken := Encoding.GetString(BytesOf(Parser.TokenString));
      if followToken = '{' then
      begin
        Inc(ParOpenCounter);
      end;
      if followToken = '}' then
      begin
        Dec(ParOpenCounter);
      end;

      if (followToken='{') and (ParOpenCounter=1) then
        Parser.SkipToken(True)
      else if (followToken='}') and (ParOpenCounter=0) then
        Parser.SkipToken(True)
      else
      begin
        Parser.CopyTokenToOutput;
        Parser.SkipToken(True);
      end;

    until ParOpenCounter = 0;

    // grab source within braces from outStream
    Result := Copy (OutStream.DataString, SavPos, OutStream.Position - SavPos+1);

    // Reset outStream reposition
    OutStream.Position := SavPos - 1;
    OutStream.Size := SavSize - 1;
  end;

// RazorContentFromStream begins here
begin
  Result := '';
  Encoding := GetEncodingOfStream(AStream, SignatureSize);
  AStream.Position := SignatureSize;
  OutStream := TStringStream.Create('', Encoding);
  try
    Parser := TCopyParser.Create(AStream, OutStream);
    try
      while True do
      begin
        while not(Parser.Token in [toEof, '@']) do
        begin
          Parser.CopyTokenToOutput;
          Parser.SkipToken(True);
        end;
        if Parser.Token = toEof then
          Break;
        if Parser.Token = '@' then
        begin
          Parser.SkipToken(True);
          TokenStr := Encoding.GetString(BytesOf(Parser.TokenString));
          Parser.SkipToken(True);

          if TokenStr = '@' then
          begin
            OutStream.WriteString('@'); // double '@@' to '@'
          end
          else if SameText (TokenStr, 'Import') then
          begin
            followToken := Encoding.GetString(BytesOf(Parser.TokenString));
            Parser.SkipToken(True);
            OutStream.WriteString(RazorContentFromTemplate
              (GetTemplatesFolder + followToken + '.html'));
          end
          else if SameText (TokenStr, 'LayoutPage') then
          begin
            followToken := Encoding.GetString(BytesOf(Parser.TokenString));
            Parser.SkipToken(True);
            ParsedTemplate := RazorContentFromTemplate
              (GetTemplatesFolder + followToken + '.html');
          end
          else if SameText (TokenStr, 'AddPage') then
          begin
            followToken := Encoding.GetString(BytesOf(Parser.TokenString));
            Parser.SkipToken(True);

            AddedPageContent := RazorContentFromTemplate
              (GetTemplatesFolder + followToken + '.html'); // ,ItemNestedLoop);

            // add page to stringlist
            FAddedPages.Add(AddedPageContent);
          end
          else if SameText (TokenStr, 'ExtraHeader') then
          begin
            // Parser.SkipToToken('{');
            blockAsString := Encoding.GetString
              (BytesOf(Parser.SkipToToken('}')));
            Parser.SkipToken(True);
            // process the block
            ParsedExtraHeader := DoBlock(RawByteString(blockAsString), Encoding);
          end
          else if SameText (TokenStr, 'RenderBody') then
          begin
            OutStream.WriteString('@RenderBody'); // leave it in
          end
          else if SameText (TokenStr, 'RenderHeader') then
          begin
            OutStream.WriteString('@RenderHeader'); // leave it in
          end
          else if SameText(TokenStr, 'RenderPages') then
          begin
            OutStream.WriteString('@RenderPages'); // leave it in
          end
          else if SameText (TokenStr, 'Scaffolding') then
          begin
            followToken := GetEntireDottedValue (Parser, Encoding);
            OutStream.WriteString(GetScaffoding(followToken));
          end
          else if SameText (TokenStr, 'LoginRequired') then
          begin
            // might be followed by the permission group
            if Parser.Token = '.' then
            begin
              Parser.SkipToken(True);
              TokenAfterDot := Encoding.GetString(BytesOf(Parser.TokenString));
              Parser.SkipToken(True);
            end;
            if not UserLoggedIn then
            begin
              raise ERlxLoginRequired.Create('LoginRequired');
            end
            else if (TokenAfterDot <> '') and (Pos (TokenAfterDot, UserRoles) <= 0) then
            begin
              raise ERlxLoginRequired.Create('NotAllowed');
            end
          end
          else if SameText (TokenStr, 'ForEach') then
          begin
            followToken := Encoding.GetString(BytesOf(Parser.TokenString));

            // new razor syntax for "ForEach" loops, with local var
            if followToken='(' then
            begin
              LoopVar := EvaluateParenthesis;

              blockAsString := FindMatchingClosingBrace;
              OutStream.WriteString (
                LoopNestedBlock (blockAsString, LoopVar, Encoding));
            end
            else
            begin
              // old razor syntax (remove?)
              Parser.SkipToToken('{');
              blockAsString := Encoding.GetString
                (BytesOf(Parser.SkipToToken('}')));
              Parser.SkipToken(True);
              // process the block
              OutStream.WriteString(LoopBlock(blockAsString, followToken, Encoding));
            end;
          end
          else if SameText (TokenStr, 'if') then
          begin
            followToken := Encoding.GetString(BytesOf(Parser.TokenString));
            if FLoopVars.ContainsKey(followToken) then
            begin
              Parser.SkipToken(True);
              LoopVar := FLoopVars.Items[followToken];
              ifValue := EvalBooleanLoopDottedValue(LoopVar, Parser, Encoding);
            end
            else
            begin
              Parser.SkipToken(True);
              ifValue := EvalBooleanDottedValue(followToken, Parser, Encoding);
            end;
            Parser.SkipToken(True);

            if Parser.Token = '{' then
            begin
              blockAsString := FindMatchingClosingBrace;
              if ifValue then
              begin
                // process the block
                OutStream.WriteString(DoBlock(RawByteString(blockAsString), Encoding));
              end;
            end
            else
              AddWarning('missing { after if');
          end
          else // not a language symbol
          begin
            // check if a local loop variable
            if FLoopVars.ContainsKey(TokenStr) then
            begin
              LoopVar := FLoopVars.Items[TokenStr];

              OutStream.WriteString(ProcessLoopDottedValue(
                LoopVar, Parser, Encoding));
              Parser.SkipToken(True);
            end
            else
            begin
              // check if any other value
              OutStream.WriteString(ProcessDottedValue(TokenStr, Parser,
                Encoding));
              Parser.SkipToken(True);
            end;
          end;
        end;
      end;
    finally
      Parser.Free;
    end;

    if ParsedTemplate <> '' then
    begin
      Result := StringReplace(ParsedTemplate, '@RenderBody',
        OutStream.DataString, []);
      Result := StringReplace(Result, '@RenderHeader',
        ParsedExtraHeader, []);

      // collect secondary pages content and place it
      AddedPagesText := '';
      for I := 0 to FAddedPages.Count-1 do
      begin
        AddedPagesText := AddedPagesText + FAddedPages.Strings[I];
      end;
      Result := StringReplace (Result, '@RenderPages', AddedPagesText, [])
    end
    else
      Result := OutStream.DataString;
  finally
    OutStream.Free;
  end;
end;

function TRlxRazorProcessor.GetSubObject(anObj: TObject; const propName: string;
  var retObj: TObject): Boolean;
var
  context: TRttiCOntext;
  aProperty: TRttiProperty;
begin
  Result := False;
  aProperty := context.GetType(anObj.ClassInfo).GetProperty (propname);
  if Assigned (aProperty) then
  begin
    if (aProperty.PropertyType.TypeKind = tkClass) then
    begin
      retObj := aProperty.GetValue(anObj).AsObject;
      Result := True;
    end;
  end
  else
    retObj := nil;
end;

function TRlxRazorProcessor.GetObjectProperty (anObj: TObject; const propName: string;
  var propValue: string): Boolean;
var
  context: TRttiCOntext;
  aProperty: TRttiProperty;
begin
  Result := False;
  if not Assigned (anObj) then
  begin
    propValue := 'Property ' + propname + ' not found in NULL object';
    Exit;
  end;
  aProperty := context.GetType(anObj.ClassInfo).GetProperty (propname);
  if Assigned (aProperty) then
  begin
    Result := True;
    if (aProperty.PropertyType.TypeKind = tkFloat) then
    begin
      if (aProperty.PropertyType.Name = 'TDate') or
        (aProperty.PropertyType.Name = 'TDateTime') or
        (aProperty.PropertyType.Name = 'TTime') then
      begin
        propValue := FormatDateTime ('dddddd',
          aProperty.GetValue(anObj).AsExtended, GetFormatSettings);
      end
      else
        propValue := FloatToStr (
          aProperty.GetValue(anObj).AsExtended, GetFormatSettings);
    end
    else
      propValue := aProperty.GetValue(anObj).ToString
  end
  else
    propValue := 'Property ' + propname + ' not found in ' +
      anObj.ClassName + ' object';
end;

function TRlxRazorProcessor.EvalBooleanObjectProperty (anObj: TObject; const propName: string;
  var Value: Boolean): Boolean;
var
  context: TRttiCOntext;
  aProperty: TRttiProperty;
begin
  Result := False;

  if not Assigned (anObj) then
  begin
    Value := False;
    Exit (False);
  end;
  aProperty := context.GetType(anObj.ClassInfo).GetProperty (propname);
  if Assigned (aProperty) then
  begin
    Result := True;
    if (aProperty.PropertyType.TypeKind = tkInteger) then
    begin
      Value := aProperty.GetValue(anObj).AsInteger <> 0; // true if not zero, false if zero
    end
    else if (aProperty.PropertyType.TypeKind in [tkString, tkWString, tkLString, tkUnicodeString, tkWideString, tkAnsiString]) then
    begin
      Value := aProperty.GetValue(anObj).AsString <> ''; // true if not empty string, false if empty
    end
    else if (aProperty.PropertyType.TypeKind in [tkChar, tkWideChar]) then
    begin
      Value := aProperty.GetValue(anObj).AsString <> #0; // true if not zero char, false if zero char
    end
    else if (aProperty.PropertyType.TypeKind = tkEnumeration) and (aProperty.PropertyType.ToString = 'Boolean') then
    begin
      Value := aProperty.GetValue(anObj).AsBoolean; // true if true
    end
    else
    begin
      Result := False;
      Value := False;
    end;
  end;
end;

function TRlxRazorProcessor.RazorContentFromTemplate(const filename: string{; ItemNestedLoop: TObject = nil}): string;
var
  fileStream: TFileStream;
begin
  Result := '';
  if not FileExists(filename) then
    raise ERazorException.Create('Missing template file ' + filename +
      ' while processing ' + fInputFilename);
  fileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := RazorContentFromStream(fileStream);
    // use the current object and settings
  finally
    fileStream.Free;
  end;
end;

procedure TRlxRazorProcessor.SetDictionaryDuplicates(
  const Value: TDictionaryDuplicates);
begin
  FDictionaryDuplicates := Value;
end;

procedure TRlxRazorProcessor.SetLanguageId(const Value: Integer);
begin
  FLanguageId := Value;
end;

procedure TRlxRazorProcessor.SetUserLoggedIn(const Value: Boolean);
begin
  FUserLoggedIn := Value;
end;

procedure TRlxRazorProcessor.SetUserRoles(const Value: string);
begin
  FUserRoles := Value;
end;

procedure TRlxRazorProcessor.SetOnLang(const Value: TRazorLanguageEvent);
begin
  FOnLang := Value;
end;

procedure TRlxRazorProcessor.SetOnScaffolding(
  const Value: TRazorScaffoldingEvent);
begin
  FOnScaffolding := Value;
end;

procedure TRlxRazorProcessor.SetRazorEngine(const Value: TRlxRazorEngine);
begin
  FRazorEngine := Value;
end;

procedure TRlxRazorProcessor.SetRazorValueEvent(const Value: TRazorValueEvent);
begin
  fRazorValueEvent := Value;
end;

procedure TRlxRazorProcessor.SetRazorWarnEvent(const Value: TRazorWarnEvent);
begin
  FRazorWarnEvent := Value;
end;

procedure TRlxRazorProcessor.SetRequest(const Value: TWebRequest);
begin
  FRequest := Value;
end;

{ TRlxRazorEngine }

procedure TRlxRazorEngine.AddToDictionary(aName: string; theObject: TObject;
  Owned: Boolean);
var
  item: TRazDictItem;
begin
  if FDataObjects.ContainsKey(aName) then
    case FDictionaryDuplicates of
      ddIgnore:
      begin
        if Owned then
          theObject.Free;
        Exit;
      end;
      ddReplace: FDataObjects.Remove(aName);
      ddError: ; // let it go, will raise an error!
    end;

  item := TRazDictItem.Create;
  if theObject is TDataSet then
    item.FKind := rdikDataset
  else
    item.FKind := rdikObject;
  item.TheObject := theObject;
  item.Owned := Owned;
  FDataObjects.Add (aName, item);
end;

procedure TRlxRazorProcessor.AddToDictionary(aName: string; aInitProc: TInitFunction);
var
  item: TRazDictItem;
begin
  if FDataObjects.ContainsKey(aName) then
    case FDictionaryDuplicates of
      ddIgnore:
      begin
        Exit;
      end;
      ddReplace: FDataObjects.Remove(aName);
      ddError: ; // let it go, will raise an error!
    end;

  item := TRazDictItem.Create;
  item.InitProcedure := aInitProc;
  item.TheObject := nil;
  item.Owned := True;
  FDataObjects.Add (aName, item);
end;

procedure TRlxRazorEngine.AddWarning(const strWarning: string);
begin
  if Assigned(FRazorWarnEvent) then
  begin
    FRazorWarnEvent(self, strWarning);
  end;
end;

procedure TRlxRazorEngine.ConnectObjectForPath(execData: TRazorExecData);
var
  pathInit: TRazorPathInitItem;
begin
  // first execute any registered page initialization code
  if PathInits.TryGetPathItem (execData.PathInfo, PathInit) then
    pathInit.DoInit (execData);

  // than also use associated event handler (old style initialization)
  if Assigned (FOnObjectForPath) then
    FOnObjectForPath (self, execData);
end;

constructor TRlxRazorEngine.Create(Owner: TComponent);
begin
  inherited;
  FDataObjects := TRazorDictionary.Create;
  FDataObjects.OnValueNotify := DictionaryItemNotification;
  FDictionaryDuplicates := ddIgnore;
  FAliases := TStringList.Create;
  FPathInits := TRazorPathInitCollection.Create;
end;

destructor TRlxRazorEngine.Destroy;
begin
  FPathInits.Free;
  FDataObjects.Free;
  FAliases.Free;
  inherited;
end;

procedure TRlxRazorEngine.DictionaryItemNotification(Sender: TObject;
  const Item: TRazDictItem; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
  begin
    if Item.Owned then
    begin
{$IFNDEF NEXTGEN}
      Item.TheObject.Free;
{$ENDIF}
      Item.TheObject := nil;
    end;
{$IFNDEF NEXTGEN}
    Item.Free;
{$ENDIF}
  end;
end;

function TRlxRazorEngine.GetLang(const FieldName: string): string;
begin
  Result := '';
  if Assigned(FOnLang) then
    FOnLang(self, FieldName, Result);
end;

class function TRlxRazorEngine.GetLangFormats(language_id: Integer): TFormatSettings;
var
  settings: TFormatSettings;
begin
  // this is duplicate in RlxWebUI.GetLangFormats
  // one of the two should be removed

  if RlxLocaleDictionary.TryGetValue (language_id, settings) then
    Result := settings
  else
    Result := FormatSettings;
end;

function TRlxRazorEngine.GetOtherValue(const ObjectName,
  FieldName: string): string;
begin
  Result := '';
  if Assigned(fRazorValueEvent) then
    fRazorValueEvent(self, ObjectName, FieldName, Result);
end;

function TRlxRazorEngine.GetScaffolding(const strDottedClass: string): string;
begin
  Result := strDottedClass; // default
  if Assigned(FOnScaffolding) then
    FOnScaffolding(self, strDottedClass, Result)
end;

function TRlxRazorEngine.GetTemplatesFolder: string;
begin
  Result := FTemplatesFolder;
  if Result = '' then
    Result := FFilesFolder;
end;

function TRlxRazorEngine.InDictionary(aName: string): Boolean;
begin
  Result := FDataObjects.ContainsKey (aName);
end;

procedure TRlxRazorEngine.ProcessPath(strPath: string; sList: TStringList);
begin
  sList.LineBreak := '/';
  sList.Text := strPath;
end;

function TRlxRazorEngine.ProcessRequest(Request: TWebRequest;
  var Found: Boolean; LoggedUser: Boolean; LanguageID: Integer; inFolder: string; userRoles: string): string;
var
  pathInfo: string;
  razorProc: TRlxRazorProcessor;
  sList: TStringList;
  //, sInFolderList, sBasePathList: TStringList;
  ext: string;
  pageInfo: TPageInfo;
  FilePath, subFolder, strAlias: string;
  execData: TRazorExecData;
  // LIndex: Integer;
begin
  pathInfo := Trim(string(Request.InternalPathInfo));
  // normalize path, remove initial /
  if (length(pathInfo) > 0) and (pathInfo[1] = '/') then
    Delete(pathInfo, 1, 1);

  sList := TStringList.Create;
  try
    // loads the path elements in a string list
    ProcessPath (pathInfo, sList);

    // get the extension and remove it from the path elements
    ext := '';
    if sList.Count > 0 then
    begin
      ext := ExtractFileExt (sList[sList.Count-1]);
      sList[sList.Count-1] := ChangeFileExt (sList[sList.Count-1], '');
    end;

    // set .htm as .html
    if ext = '.htm' then
      ext := ext + 'l'; // TODO: actual file extension as a property

    if BasePath <> '' then
      StripPath(sList, BasePath);

    if sList.Count = 0 then
      sList.Add(FHomePage); // TODO: make a property, with default

    if sList.Count = 0 then // if this is still empty
      raise Exception.Create ('Cannot process request with no path and no homepage');

    pageInfo := TPageInfo.Create;
    try
      // page information
      pageInfo.Referer := string(Request.Referer);
      pageInfo.Browser := string(Request.UserAgent); // clean up and make usable
      pageInfo.Address := string(Request.RemoteAddr);

      // user information
      pageInfo.UserLogged := LoggedUser;
      pageInfo.UserRole := userRoles;

      // process from specific folder and remove it
      StripPath(sList, inFolder);

      subFolder := '';
      // direct page request, go for the HTML with no params
      // all files must be in the root HTML folder
      if sList.Count = 1 then
      begin
        pageInfo.Page :=  sList[0];
      end
      // handles type/object.html requests and folder/file requests
      else if sList.Count = 2 then
      begin
        if DirectoryExists (FFilesFolder + sList[0]) then
        begin
          pageInfo.Page := sList[1];
          subFolder := sList[0] + '/';
        end
        else
        begin
          pageInfo.Page := sList[0];
          pageInfo.Item := sList[1];
          pageInfo.Action := 'view'; // default action
        end;
      end
      // handles action/type/object.html requests
      else if sList.Count = 3 then
      begin
        pageInfo.Action := sList[0];
        pageInfo.Page := sList[1];
        pageInfo.Item := sList[2];
      end
      // cannot have more than 3 params for now
      else
      begin
        Result := '';
  //        Response.StatusCode := 404;
  //        Response.ReasonString := 'File not found';
        Exit;
      end;

      // look for alias, if found replace the page name
      strAlias := FAliases.Values [pageInfo.Page];
      if strAlias <> '' then
        pageInfo.Page := strAlias;

      // HTML file to read
      if SameText (ext, '.js') then
        // TODO: add a property in the component at least with the base path
        // or with a path file mapping
        FilePath := ChangeFileExt ('../' + inFolder + '/' + pageInfo.Page, '.js')
      else
        FilePath := ChangeFileExt (FFilesFolder + subFolder + pageInfo.Page, '.html');

      if not FileExists(FilePath) then
      begin
        if Assigned (FOnPageError) then
          FOnPageError (self, pageInfo);

        if FErrorPage <> '' then
        begin
          FilePath := FFilesFolder + FErrorPage;
          if not FileExists(FilePath) then
          begin
            Found := False;
            Result := '';
            Exit;
          end;
        end;
      end;

      Found := True;
      razorProc := TRlxRazorProcessor.Create(nil);
      try
        if Assigned (FOnPageLog) then
          FOnPageLog (self, pageInfo);
        razorProc.RazorEngine := self;
        razorProc.InputFilename := FilePath;
        razorProc.UserLoggedIn := LoggedUser;
        razorProc.UserRoles := userRoles;
        razorProc.LanguageID := LanguageID;
        razorproc.AddToDictionary ('page', pageInfo, False);
        execData.PathInfo := pageInfo.Page;
        execData.PathParam := pageInfo.Item;
        execData.razorProc := razorProc;
        ConnectObjectForPath (execData);
        razorProc.Request := Request; // passed manually
        Result := razorProc.Content;
      finally
        razorProc.Free;
      end;
    finally
      pageInfo.Free;
    end;
  finally
    sList.Free;
  end;
end;

procedure TRlxRazorEngine.SetAliases(const Value: TStrings);
begin
  FAliases.Assign (Value);
end;

procedure TRlxRazorEngine.SetDictionaryDuplicates(
  const Value: TDictionaryDuplicates);
begin
  FDictionaryDuplicates := Value;
end;

procedure TRlxRazorEngine.SetErrorPage(const Value: string);
begin
  FErrorPage := Value;
end;

procedure TRlxRazorEngine.SetFilesFolder(const Value: string);
begin
  FFilesFolder := Value;
end;

procedure TRlxRazorEngine.SetHomePage(const Value: string);
begin
  FHomePage := Value;
end;

procedure TRlxRazorEngine.SetOnLang(const Value: TRazorLanguageEvent);
begin
  FOnLang := Value;
end;

procedure TRlxRazorEngine.SetOnObjectForPath(const Value: TRazorObjectOnPath);
begin
  FOnObjectForPath := Value;
end;

procedure TRlxRazorEngine.SetOnScaffolding(const Value: TRazorScaffoldingEvent);
begin
  FOnScaffolding := Value;
end;

procedure TRlxRazorEngine.SetPathInits(const Value: TRazorPathInitCollection);
begin
  FPathInits := Value;
end;

procedure TRlxRazorEngine.SetRazorValueEvent(const Value: TRazorValueEvent);
begin
  fRazorValueEvent := Value;
end;

procedure TRlxRazorEngine.SetRazorWarnEvent(const Value: TRazorWarnEvent);
begin
  FRazorWarnEvent := Value;
end;

procedure TRlxRazorEngine.SetTemplatesFolder(const Value: string);
begin
  FTemplatesFolder := Value;
end;

procedure TRlxRazorEngine.StripPath(sPath: TStringList; const PathToStrip: string);
var
  sPathToStrip: TStringList;
  LIndex: Integer;
begin
  sPathToStrip := TStringList.Create;
  try
    ProcessPath (PathToStrip, sPathToStrip);
    if sPathToStrip.Count > 0 then
      for LIndex := 0 to sPathToStrip.Count-1 do
        if (sPath.Count > 0) and SameText(sPathToStrip[LIndex], sPath[0]) then
          sPath.Delete(0);
  finally
    sPathToStrip.Free;
  end;
end;

{ TRazDictItem }

procedure TRazDictItem.SetInitProcedure(const Value: TInitFunction);
begin
  FInitProcedure := Value;
end;

procedure TRazDictItem.SetKind(const Value: TRazDictItemKind);
begin
  FKind := Value;
end;

procedure TRazDictItem.SetOwned(const Value: Boolean);
begin
  FOwned := Value;
end;

procedure TRazDictItem.SetTheObject(const Value: TObject);
begin
  FTheObject := Value;
end;

{ TPageInfo }

function TPageInfo.GetUserNotLogged: Boolean;
begin
  Result := not FUserLogged;
end;

procedure TPageInfo.SetAction(const Value: string);
begin
  FAction := Value;
end;

procedure TPageInfo.SetAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TPageInfo.SetBrowser(const Value: string);
begin
  FBrowser := Value;
end;

procedure TPageInfo.SetItem(const Value: string);
begin
  FItem := Value;
end;

procedure TPageInfo.SetPage(const Value: string);
begin
  FPage := Value;
end;

procedure TPageInfo.SetReferer(const Value: string);
begin
  FReferer := Value;
end;

procedure TPageInfo.SetUserRole(const Value: string);
begin
  FUserRole := Value;
end;

procedure TPageInfo.SetUserLogged(const Value: Boolean);
begin
  FUserLogged := Value;
end;

procedure TPageInfo.SetUserNotLogged(const Value: Boolean);
begin
  FUserLogged := not Value;
end;

{ TRazLoopVar }

constructor TRazLoopVar.Create(aRazProc: TRlxRazorProcessor; const aLoopObjToken, aLoopVarToken, aLoopObjAfterDotToken: string);
begin
  inherited Create;
  FRazProc := aRazProc;
  FLoopObjTokenStr := aLoopObjToken;
  FLoopObjTokenAfterDotStr := aLoopObjAfterDotToken;
  FLoopVarTokenStr := aLoopVarToken;
end;

function TRazLoopVar.InitLoopObjects: Boolean;
var
  refObj, refSubObj: TObject;
begin
  Result := False;
  // init internal var
  refSubObj := nil;
  // access the main object
  refObj := FRazProc.GetLoopOrDictionaryObject (FLoopObjTokenStr, FLoopVarTokenStr);
  if not Assigned (refObj) then
  begin
    FRazProc.AddWarning('Loop object not found: ' + FLoopObjTokenStr);
    Exit (False);
  end;

  // if we are accessing a subobject, let's grab it instead
  if FLoopObjTokenAfterDotStr = EmptyStr then
  begin
    FLoopObject := refObj;
    Result := True;
  end
  else
  begin
    refObj := FRazProc.GetCurrenctObject(FLoopObjTokenStr);

    if Assigned(refObj) then
    begin

      if not FRazProc.GetSubObject(refObj, FLoopObjTokenAfterDotStr, refSubObj) then  // M.M.
      begin
        FRazProc.AddWarning('Loop suboject ' + FLoopObjTokenAfterDotStr + ' not found for ' + FLoopVarTokenStr);
        Exit (False);
      end
      else
      begin
        // ok, use subject as loop object
        FLoopObject := refSubObj;
        Result := True;
      end;
    end
    else FRazProc.AddWarning('Loop oject not found for object var ' + FLoopObjTokenStr);

  end;

  FCurrPos := -1; // used also for datasets
end;

function TRazLoopVar.NextLoopObject: Boolean;
begin
  Result := True; // there is more
  Inc (FCurrPos);

  if (FLoopObject is TList <TObject>) then
  begin
    if (FCurrPos >= TList <TObject>(FLoopObject).Count) then
      Result := False // done
    else
      CurrentObj := TList <TObject>(FLoopObject) [FCurrPos];
  end
  else if FLoopObject is TDataSet then
  begin
    if FCurrPos = 0 then
    begin
      TDataSet(FLoopObject).Open;
      TDataSet(FLoopObject).First;
      if TDataSet(FLoopObject).EOF then
        Result := False; // done
    end
    else
    begin
      TDataSet(FLoopObject).Next;
      if TDataSet(FLoopObject).EOF then
        Result := False; // done
    end;
  end;
end;

procedure TRazLoopVar.SetCurrentObj(const Value: TObject);
begin
  FCurrentObj := Value;
end;

procedure TRazLoopVar.SetLoopObject(const Value: TObject);
begin
  FLoopObject := Value;
end;

{ TRazorPathInitItem }

procedure TRazorPathInitItem.DoInit (execData: TRazorExecData);
begin
  if Assigned (FOnPathInit) then
    FOnPathInit (self, execData);
end;

procedure TRazorPathInitItem.SetOnPathInit(const Value: TRazorPathInitEvent);
begin
  FOnPathInit := Value;
end;

procedure TRazorPathInitItem.SetPath(const Value: string);
begin
  FPath := Value;
end;

{ TRazozPathInitCollection }

function TRazorPathInitCollection.Add: TRazorPathInitItem;
begin
  result := TRazorPathInitItem (inherited Add);
end;

function TRazorPathInitCollection.AddPathItem(const Path: string;
  InitMethod: TRazorPathInitEvent): TRazorPathInitItem;
begin
  Result := Add;
  Result.Path := Path;
  Result.OnPathInit := InitMethod;
end;

constructor TRazorPathInitCollection.Create;
begin
  inherited Create (TRazorPathInitItem);
end;

function TRazorPathInitCollection.GetItem(Index: Integer): TRazorPathInitItem;
begin
  Result := TRazorPathInitItem (inherited GetItem (Index));
end;

procedure TRazorPathInitCollection.SetItem(Index: Integer;
  const Value: TRazorPathInitItem);
begin
  inherited SetItem (Index, Value);
end;

function TRazorPathInitCollection.TryGetPathItem(const Path: string;
  var pathItem: TRazorPathInitItem): Boolean;
var
  I: Integer;
begin
  pathItem := nil;
  Result := False;

  for I := 0 to Count - 1 do
  begin
    if SameText (Items[I].Path, Path) then  // TODO: more flexible mathing needed?
    begin
      pathItem := Items[I];
      Result := True;
    end;
  end;
end;

end.

