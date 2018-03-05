#!/usr/bin/env Rscript
HTTPS_PORT = 8002
n = 10

message(sprintf("QUERING Fibonacci number for %d\n", n))

n_fib = system2("curl", sprintf('-k  -H "X-API-KEY: d1d59815-515d-46b8-b890-b57439070e38" -X GET "https://localhost:%d/fib?n=%d"', HTTPS_PORT, n), stdout = TRUE)

message(sprintf("\nANSWER: %s", n_fib))
