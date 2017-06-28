program ClientScript;

uses
  Forms,
  ClientMainForm in 'ClientMainForm.pas' {DataAccessClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataAccessClientForm, DataAccessClientForm);
  Application.Run;
end.
