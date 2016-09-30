unit RlxWebModule;

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
///    Marco Cantu                                              ///
///                                                             ///
///////////////////////////////////////////////////////////////////

interface

uses
  HTTPApp, Classes, RlxRazor, RlxConfiguration;

// uses a class interceptor to avoid desing time problems in case
// you are not using TWebModule

type
  TWebModule = class (HTTPApp.TWebModule, IWebRequestHandler)
  published
//    rlxDSHTTPWebDispatcher: TDSHTTPWebDispatcher;
    rlxRazorEngine: TRlxRazorEngine;
  private
    // session related data, re-computed for each incoming request
    FSessionID: string;
//    FSessionData: TBaseSessionData;
    FUtf8Result: Boolean;

    procedure RazorScaffolding(Sender: TObject; const qualifClassName: string;
      var ReplaceText: string);
//    procedure ReadSessionInfo (Request: TWebRequest; Response: TWebResponse);
    procedure SetUtf8Result(const Value: Boolean);
    procedure ConvertResponseToUTF8(Response: TWebResponse;
      const ResponseContent: String);
  protected
    // session related properties
    property SessionID: string read FSessionID;
//    property SessionData: TBaseSessionData read FSessionData;
    property Utf8Result: Boolean read FUtf8Result write SetUtf8Result;
  public
    constructor Create (Owner: TComponent); override;
    destructor Destroy; override;
    procedure OnRelaxDefaultAction (Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  public
    QueryParams: TStringList;
    function HandleRequest(Request: TWebRequest; Response: TWebResponse): Boolean;
  end;

implementation

uses
  {RlxDynamicServerClass,} SysUtils{, RlxWebUI};

{ TWebModule }

constructor TWebModule.Create(Owner: TComponent);
var
  aWebAction: TWebActionItem;
begin
  inherited Create(Owner);

//  // create the HTTP dispatcher, using some global settings
//  rlxDSHTTPWebDispatcher := TDSHTTPWebDispatcher.Create(self);
//  rlxDSHTTPWebDispatcher.Server := RlxDataSnap.rlxDSServer;
//  rlxDSHTTPWebDispatcher.SessionTimeout := TSessionsData.SessionTimeOut;
//  rlxDSHTTPWebDispatcher.WebDispatch.MethodType := mtAny;
//  rlxDSHTTPWebDispatcher.WebDispatch.PathInfo := 'datasnap*';
//  rlxDSHTTPWebDispatcher.AuthenticationManager := RlxDataSnap.rlxDSAuthenticationManager;

  // create the razor engine
  rlxRazorEngine := TRlxRazorEngine.Create(self);
  rlxRazorEngine.FilesFolder := ServerConf['razor:files_folder'];
  rlxRazorEngine.TemplatesFolder := ServerConf['razor:templates_folder'];
  rlxRazorEngine.HomePage := ServerConf['pages:homepage'];
  rlxRazorEngine.ErrorPage := ServerConf['pages:errorpage'];
  ServerConf.ReadSectionValues('aliases', rlxRazorEngine.Aliases);
  rlxRazorEngine.OnScaffolding := RazorScaffolding;

  // install default event handler in Web Module
  aWebAction := Actions.Add;
  aWebAction.PathInfo := '/relaxdefaultaction';
  aWebAction.Default := True;
  aWebAction.OnAction := OnRelaxDefaultAction;

  QueryParams := TStringList.Create; // used by jqGrid and other custom processing
end;

destructor TWebModule.Destroy;
begin
  QueryParams.Free;
  inherited;
end;

//procedure TWebModule.ReadSessionInfo (Request: TWebRequest; Response: TWebResponse);
//begin
//  if FSessionID = '' then
//  begin
//    FSessionID := sessionsData.GetOrCreateSessionID(Request,
//      Response, RlxDataSnap.rlxDSAuthenticationManager);
//  end;
//end;

procedure TWebModule.SetUtf8Result(const Value: Boolean);
begin
  FUtf8Result := Value;
end;

function TWebModule.HandleRequest(Request: TWebRequest;
  Response: TWebResponse): Boolean;
begin
  // reset the data, given the web module is recycled
  FSessionID := '';
//  FSessionData := nil;
  // initialize session if needed and read sessiond data
  // LanguageID and other info used by HTML/JS translations and processing
//  ReadSessionInfo (Request, Response);
//  FSessionData := SessionsData.Items[FSessionid];

  // manage params for jqGrid
  if Request.QueryFields.Count > 2 then
  begin
    QueryParams.Assign(Request.QueryFields);
    Request.QueryFields.Clear;
  end
  else
    QueryParams.Clear;

  // add sessionid to request so that REST methods will use it
  Request.QueryFields.Add('SESSIONID=' + FSessionID);

  Result := inherited HandleRequest (Request, Response);
end;

procedure TWebModule.ConvertResponseToUTF8(Response: TWebResponse;
  const ResponseContent: String);
var
  aStream: TMemoryStream;
  aStreamWriter: TStreamWriter;
begin
  aStream := TMemoryStream.Create;
  aStreamWriter := TStreamWriter.Create (aStream, TEncoding.UTF8);
  try
    aStreamWriter.Write(ResponseContent);
    aStream.Position := 0;
    Response.ContentType := 'text/html; charset=utf-8';
    Response.ContentStream := aStream;
  finally
    aStreamWriter.Free;
  end;
end;


procedure TWebModule.OnRelaxDefaultAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Found: Boolean;
  stringResponse: string;
begin
  Found := False;
  try
    if QueryParams.Count > 0 then
      Request.QueryFields.Assign (QueryParams);

    stringResponse := rlxRazorEngine.ProcessRequest(Request, Found,
      False {FSessionData.LoggedIn}, 1 {FSessionData.LanguageID}, '', '' {sessionData.Role});
    if Utf8Result then
      ConvertResponseToUTF8 (Response, stringResponse)
    else
      Response.Content := stringResponse;
  except
    on E: ERlxLoginRequired do
    begin
//      if Assigned(sessionData) then
//        FSessionData.PageBeforeLogin := Trim(string(Request.InternalPathInfo)); // TODO: aggiungere parametri
      Response.SendRedirect(ServerConf['pages:loginpage']);
    end;
    on E: Exception do
    begin
      Response.StatusCode := 500;
      Response.ReasonString := 'Server error';
    end;
  end;
  if not Found then
  begin
      Response.StatusCode := 303;
      Response.ReasonString := 'Not found';
  end;
end;

procedure TWebModule.RazorScaffolding(Sender: TObject;
  const qualifClassName: string; var ReplaceText: string);
begin
  // this is quite a hack, we probably need an extended syntax for this
//  if Pos ('View_', qualifClassName) = 1 then
//    ReplaceText := HtmlFormForClass(Copy (qualifClassName, 6, maxint), True)
//  else
//    ReplaceText := HtmlFormForClass(qualifClassName); // form-based HTML
end;


initialization
  // load some global settings
//  TUserManagement.PageAfterLogin := ServerConf['pages:pageafterlogin'];
//  TBaseSessionData.GuestUserName := ServerConf['users:guest_username'];

end.
