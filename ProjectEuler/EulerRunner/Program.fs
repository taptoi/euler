open ProjectEuler
open System.Diagnostics

[<EntryPoint>]
let main argv = 
    let input = Array.head argv
    let printRes (res, prob) = printfn "Result for %s is: %s" prob res
    let printBenchmark time = printfn "Time: %f ms" time 
    let solve prob = 
                   let result = match prob with
                                | "P1" -> P1.solve.ToString()
                                | "P2" -> P2.solve.ToString()
                                | "P3" -> P3.solve.ToString()
                                | "P4" -> P4.solve.ToString()
                                | "P5" -> P5.solve.ToString()
                                | "P6" -> P6.solve.ToString()
                                | "P7" -> P7.solve.ToString()
                                | "P8" -> P8.solve.ToString()
                                | "P9" -> P9.solve.ToString()
                                | "P10" -> P10.solve.ToString()
                                | "P11" -> P11.solve.ToString()
                                | "P12" -> P12.solve.ToString()
                                | "P13" -> P13.solve.ToString()
                                | "P14" -> P14.solve.ToString()
                                | "P15" -> P15.solve.ToString()
                                | "P16" -> P16.solve.ToString()
                                | "P17" -> P17.solve.ToString()
                                | "P18" -> P18.solve.ToString()
                                | "P19" -> P19.solve.ToString()
                                | "P20" -> P20.solve.ToString()
                                | "P21" -> P21.solve.ToString()
                                | "P22" -> P22.solve.ToString()
                                | "P23" -> P23.solve.ToString()
                                | "P67" -> P67.solve.ToString()
                                | _ -> failwith "Unknown problem"
                   (result, prob)
    let stopWatch = Stopwatch.StartNew () 
    solve input |> printRes
    printBenchmark stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
