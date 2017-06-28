unit ClientMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, IniFiles,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.StorageBin;

type
  TDataAccessClientForm = class(TForm)
    Memo1: TMemo;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    MemoLog: TMemo;
    btnScript: TButton;
    Memo2: TMemo;
    btnScript2: TButton;
    FDMemTable1: TFDMemTable;
    procedure btnScriptClick(Sender: TObject);
    procedure btnScript2Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  DataAccessClientForm: TDataAccessClientForm;

implementation

{$R *.dfm}

uses
  DateUtils, RlxRazor;

type
  TSimpleClass = class
  private
    FValue: Integer;
    procedure SetValue(const Value: Integer);
  published
    constructor Create (aValue: Integer);
    property Value: Integer read FValue write SetValue;

  end;

procedure TDataAccessClientForm.btnScript2Click(Sender: TObject);
var
  Razor1: TRlxRazorProcessor;
begin
  Razor1 := TRlxRazorProcessor.Create(self);

  Razor1.AddToDictionary('data', FDMemTable1, False);

  MemoLog.Lines.Add (
    Razor1.DoBlock (Memo2.Lines.Text));

  Razor1.Free;
end;

procedure TDataAccessClientForm.btnScriptClick(Sender: TObject);
var
  Razor1: TRlxRazorProcessor;
begin
  Razor1 := TRlxRazorProcessor.Create(self);

  Razor1.AddToDictionary('myemp', FDMemTable1, False);
  Razor1.AddToDictionary('form', self, False);
  Razor1.AddToDictionary('myclass', TSimpleClass.Create(42));

  MemoLog.Lines.Add (
    Razor1.DoBlock (Memo1.Lines.Text));

  Razor1.Free;
end;

{ TSimpleClass }

constructor TSimpleClass.Create(aValue: Integer);
begin
  fValue := aValue;
end;

procedure TSimpleClass.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

end.
