local threads = {}

local counter = 1

function setup(thread)
    thread:set("id", counter)
    table.insert(threads, thread)
    counter = counter + 1
end

function init(args)
    requests  = 0
    responses = 0
    success = 0
    
    local msg = "thread %d created"
    print(msg:format(id))
end

function request()
    requests = requests + 1
    return wrk.request()
end

function response(status, headers, body)
    responses = responses + 1
    if status == success then
        success = success + 1
    end
    io.write(string.format("%d,%d,%d\n", id, responses, status))
end

done = function(summary, latency, requests)
    io.write("Latency\n")
    for counter=1,#latency do
        value, count = latency(counter)
        io.write(string.format("%d,%d,%d\n", counter, value, count))
    end
    for index, thread in ipairs(threads) do
        local id        = thread:get("id")
        local requests  = thread:get("requests")
        local responses = thread:get("responses")
        local success   = thread:get("success")
        local msg = "thread %d made %d requests and got %d responses\n"
        io.write(msg:format(id, requests, responses))
    end    
end
