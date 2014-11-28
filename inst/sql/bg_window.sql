SELECT
  OrderProcedureID
, Rb.CommunityPatientID
, Rb.CommunityPatientEncounterID
, CONVERT(varchar(16), Proc2b.SpecimenTakenTimeDTS) as ObtainedDTS
, Cb.ComponentNM
, ResultValueNBR
, left(DepartmentNM, 3) as facility
, DepartmentNM

FROM
  ProvidenceEpic.Orders.ResultBASE Rb WITH (NOLOCK)
  INNER JOIN ProvidenceEpic.Reference.ComponentBASE Cb
  on Cb.ComponentID = Rb.ComponentID
  LEFT JOIN ProvidenceEpic.Orders.Procedure2BASE Proc2b
    ON Proc2b.CommunityOrderProcedureID = Rb.OrderProcedureID
  INNER JOIN ProvidenceEpic.Reference.DepartmentBASE Db
    ON Db.DepartmentID = Proc2b.PatientLocationID

WHERE 1=1
  AND ComponentNM IN ('POC GLUCOSE', 'GLUCOSE')
  AND Rb.CommunityPatientID = '%s'
