cth <-
structure(list(name = "logic", type = "module"), .Names = c("name",
"type"))
csh <-
structure(list(name = "#", bits = "8", humanReadableName = "data",
    type = "wire", `#.0` = structure(list(name = "#.0", bits = 1,
        humanReadableName = "data, bit 0", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.1` = structure(list(
        name = "#.1", bits = 1, humanReadableName = "data, bit 1",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.2` = structure(list(name = "#.2", bits = 1,
        humanReadableName = "data, bit 2", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.3` = structure(list(
        name = "#.3", bits = 1, humanReadableName = "data, bit 3",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.4` = structure(list(name = "#.4", bits = 1,
        humanReadableName = "data, bit 4", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.5` = structure(list(
        name = "#.5", bits = 1, humanReadableName = "data, bit 5",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.6` = structure(list(name = "#.6", bits = 1,
        humanReadableName = "data, bit 6", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.7` = structure(list(
        name = "#.7", bits = 1, humanReadableName = "data, bit 7",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type"))), .Names = c("name", "bits", "humanReadableName",
"type", "#.0", "#.1", "#.2", "#.3", "#.4", "#.5", "#.6", "#.7"
))
cah <-
structure(list(name = "logic", type = "module", `#` = structure(list(
    name = "#", bits = "8", humanReadableName = "data", type = "wire",
    `#.0` = structure(list(name = "#.0", bits = 1, humanReadableName = "data, bit 0",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.1` = structure(list(name = "#.1", bits = 1,
        humanReadableName = "data, bit 1", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.2` = structure(list(
        name = "#.2", bits = 1, humanReadableName = "data, bit 2",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.3` = structure(list(name = "#.3", bits = 1,
        humanReadableName = "data, bit 3", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.4` = structure(list(
        name = "#.4", bits = 1, humanReadableName = "data, bit 4",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.5` = structure(list(name = "#.5", bits = 1,
        humanReadableName = "data, bit 5", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type")), `#.6` = structure(list(
        name = "#.6", bits = 1, humanReadableName = "data, bit 6",
        type = "wire"), .Names = c("name", "bits", "humanReadableName",
    "type")), `#.7` = structure(list(name = "#.7", bits = 1,
        humanReadableName = "data, bit 7", type = "wire"), .Names = c("name",
    "bits", "humanReadableName", "type"))), .Names = c("name",
"bits", "humanReadableName", "type", "#.0", "#.1", "#.2", "#.3",
"#.4", "#.5", "#.6", "#.7")), `$` = structure(list(name = "$",
    bits = "1", humanReadableName = "data_valid", type = "wire"), .Names = c("name",
"bits", "humanReadableName", "type")), `%` = structure(list(name = "%",
    bits = "1", humanReadableName = "en", type = "wire"), .Names = c("name",
"bits", "humanReadableName", "type")), `&` = structure(list(name = "&",
    bits = "1", humanReadableName = "rx_en", type = "wire"), .Names = c("name",
"bits", "humanReadableName", "type")), `'` = structure(list(name = "'",
    bits = "1", humanReadableName = "tx_en", type = "wire"), .Names = c("name",
"bits", "humanReadableName", "type")), `(` = structure(list(name = "(",
    bits = "1", humanReadableName = "empty", type = "wire"), .Names = c("name",
"bits", "humanReadableName", "type")), `)` = structure(list(name = ")",
    bits = "1", humanReadableName = "underrun", type = "wire"), .Names = c("name",
"bits", "humanReadableName", "type"))), .Names = c("name", "type",
"#", "$", "%", "&", "'", "(", ")"))

counts.topref <-
structure(list(hierarchy = cth, counts = structure(list(
    logic = structure(list(`0` = structure(c(4L,1L, 2L, 1L), .Names = c("0",
    "2211", "2296", "2302")), `1` = structure(c(2L, 1L), .Names = c("0",
    "2296")), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x"))), .Names = "logic"), timestamps = c("0",
"2211", "2296", "2302", "2303")), .Names = c("hierarchy", "counts",
"timestamps"))
counts.allref <-
structure(list(hierarchy = cah, counts = structure(list(
    `'` = structure(list(`0` = structure(1L, .Names = "2211"),
        `1` = structure(1L, .Names = "0"), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.0` = structure(list(`0` = structure(1L, .Names = "2296"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.1` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.2` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.3` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.4` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.5` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.6` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.7` = structure(list(`0` = structure(1L, .Names = "2296"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `$` = structure(list(`0` = structure(c(1L,
    1L), .Names = c("0", "2302")), `1` = structure(1L, .Names = "2296"),
        z = structure(integer(0), .Names = character(0)), x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `%` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(1L, .Names = "0"), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `&` = structure(list(`0` = structure(1L, .Names = "0"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `(` = structure(list(`0` = structure(1L, .Names = "0"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `)` = structure(list(`0` = structure(1L, .Names = "0"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), logic = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x"))), .Names = c("'", "#", "#.0", "#.1", "#.2",
"#.3", "#.4", "#.5", "#.6", "#.7", "$", "%", "&", "(", ")", "logic"
)), timestamps = c("0", "2211", "2296", "2302", "2303")), .Names = c("hierarchy",
"counts", "timestamps"))
counts.subref <-
structure(list(hierarchy = data.tree::FromListSimple(csh), counts = structure(list(
    `#` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.0` = structure(list(`0` = structure(1L, .Names = "2296"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.1` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.2` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.3` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.4` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.5` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.6` = structure(list(`0` = structure(integer(0), .Names = character(0)),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x")), `#.7` = structure(list(`0` = structure(1L, .Names = "2296"),
        `1` = structure(integer(0), .Names = character(0)), z = structure(integer(0), .Names = character(0)),
        x = structure(integer(0), .Names = character(0))), .Names = c("0",
    "1", "z", "x"))), .Names = c("#", "#.0", "#.1", "#.2", "#.3",
"#.4", "#.5", "#.6", "#.7")), timestamps = c("0", "2211", "2296",
"2302", "2303")), .Names = c("hierarchy", "counts", "timestamps"
))
