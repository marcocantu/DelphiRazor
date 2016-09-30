program RazorWebBroker;
{$APPTYPE GUI}

uses
  Forms,
  WebReq,
  IdHTTPWebBrokerBridge,
  RazorWeb_MainForm in 'RazorWeb_MainForm.pas' {Form12},
  RazorWeb_WebModule in 'RazorWeb_WebModule.pas' {WebModule13: TWebModule};

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
