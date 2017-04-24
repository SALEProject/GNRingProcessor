program GNRingProcessor;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, bitVariants, bitDataSource, bitMSSQLDataConnector, MainGNRingProcessor,
 GNRingProcessorVars, GNProcessor
 { you can add units after this };

{$R *.res}

begin
 RequireDerivedFormResource := True;
 Application.Initialize;
 Application.CreateForm(Tfrm_MainForm, frm_MainForm);
 Application.Run;
end.

