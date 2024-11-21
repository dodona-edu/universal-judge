import * as fs from "fs";

function echoFile(content: Object) {
    return new Promise((resolve: (value: unknown) => void, reject: (reason?: any) => void) => {
        fs.readFile(content, {encoding:'utf8', flag:'r'}, (err: any, data: unknown) => {
            if (err) {
                reject(err);
            } else {
                resolve(data);
            }
        });
    }).then((c: string) => c.trim());
}
