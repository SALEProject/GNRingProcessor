unit MainGNRingProcessor;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
 DBGrids, StdCtrls, ExtCtrls, ComCtrls, Spin, GNRingProcessorVars, db,
 bitVariants, DateUtils, DOM, XMLRead, XMLWrite, GNProcessor;


type

 { Tfrm_MainForm }

 Tfrm_MainForm = class(TForm)
  Label1: TLabel;
  ed_MatchingInterval: TSpinEdit;
  ed_Exceptions: TMemo;
  tmr: TTimer;
  procedure ed_MatchingIntervalChange(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure tmrTimer(Sender: TObject);
 private
  { private declarations }
 public
  { public declarations }

  processor: TGNProcessor;
 end;

var
 frm_MainForm: Tfrm_MainForm;

implementation

{$R *.lfm}

{ Tfrm_MainForm }


procedure Tfrm_MainForm.FormCreate(Sender: TObject);
var StatusFileName, SettingsFileName: shortstring;
begin
 SettingsFileName:= ParamStr(0);
 SettingsFileName:= IncludeTrailingPathDelimiter(ExtractFilePath(SettingsFileName)) +
                    ExtractFileNameOnly(SettingsFileName) + '.xml';
 if FileExists(SettingsFileName) then Settings:= LoadGNRingProcessorSettings(SettingsFileName)
 else Application.Terminate;

 InitDS;

 ed_MatchingInterval.Value:= tmr.Interval;

 StatusFileName:= ParamStr(0);
 StatusFileName:= IncludeTrailingPathDelimiter(ExtractFilePath(StatusFileName)) +
                  ExtractFileNameOnly(StatusFileName) + '_status.xml';
 processor:= TGNProcessor.Create(StatusFileName);
 processor.DataSource:= ds_brm;

 processor.Refresh_Orders;
 //Refresh_TreeOrders;
 //Paint_Orders;
end;

procedure Tfrm_MainForm.tmrTimer(Sender: TObject);
begin
 processor.Refresh_Orders;

 try
  processor.Process;

 except
  on E: Exception do ed_Exceptions.Lines.Add(E.Message);
 end;

 processor.WriteStatus;
end;

procedure Tfrm_MainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure Tfrm_MainForm.ed_MatchingIntervalChange(Sender: TObject);
begin
 tmr.Interval:= ed_MatchingInterval.Value;
end;

end.

