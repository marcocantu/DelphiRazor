unit RlxCompsRegistration;

interface

procedure Register;

implementation

uses
  Classes, {RlxTableModule, }RlxRazor;

procedure Register;
begin
  RegisterComponents('Razor', [TRlxRazorProcessor, TRlxRazorEngine]);
  // RegisterComponents ('Relax', [TRlxQueryGateway, TRlxTableGateway, TRlxTableModule]);
end;


end.
