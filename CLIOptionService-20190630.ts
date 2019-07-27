// commander CLI option helper.
/* example:
// where you define the Command() object:
main
    .command('compileSingle <file>')
    .description('compile single file')
    .option('-v, --version <ver>')
    .option('-U, --no-upload')
    .option('-m, --verbose')
    .action(
        withCLIOptions(
            'version', 'upload', 'verbose',
        )(
            CLI.compileSingle
        )
    );
    
// then, at where you define CLI.compileSingle:
const compileSingle = (file: string) => {
    // blahblahblah...
}

// uses getCLIOption() to retrieve options.
*/

let CLI_OPTIONS: {[key: string]: any} = {};

export
const getCLIOption = (key: string) => {
    return CLI_OPTIONS[key];
}

export
const setCLIOption = (key: string, value: any) => {
    CLI_OPTIONS[key] = value;
}

export
const withCLIOptions = (...options: string[]) => (f: (...args: any[]) => any) => (...args: any[]) => {
    options.forEach((v) => setCLIOption(v, (args as any)[args.length - 1][v]));
    return f(...args);
};

export
const onCLIOption = (optionName: string, callback: () => any) => {
    if (CLI_OPTIONS[optionName]) {
        callback();
    }
}

export
const onVerboseDebugInfo = (msg: any, type?: string) => onCLIOption('verbose', () => ((console as any)[type? type : 'log'])(msg));
