unit WebModuleUnit;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.StorageBin, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, RlxRazor;

type
  TWebModule2 = class(TWebModule)
    RlxRazorProcessor1: TRlxRazorProcessor;
    FDMemTable1: TFDMemTable;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    procedure WebModule2DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule2WebActionItem1Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule2;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModule2.WebModule2DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule2.WebModule2WebActionItem1Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  filename: string;
begin
  filename := './employee.fds';
  if not FDMemTable1.Active then
  begin
    FDMemTable1.LoadFromFile(filename);
    FDMemTable1.Open;
  end;

  RlxRazorProcessor1.AddToDictionary('employee', FDMemTable1, False);
  Response.Content := RlxRazorProcessor1.Content;
end;

end.
