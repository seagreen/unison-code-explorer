/**
 * Static-site documentation generator!
 *
 * This is in javascript for ease-of-hacking, will translate into haskell pretty soon.
 *
 * Usage:
 * ```
 * $ stack run -- --json --directory path/to/unison/project > dump.json
 * $ node compile.js dump.json path/to/output/directory
 * ```
 *
 * NOTE: This is very much at the proof-of-concept stage.
 */
const path = require('path');
const fs = require('fs');

const [_, __, jsonFile, destDir] = process.argv;

const data = require(path.resolve(jsonFile));

const byHash = {};
const byName = {};
data.forEach((item) => {
    byName[item.name.primaryName] = item;
    byHash[item.name.rhash] = item;
});
const names = Object.keys(byName);

const style = `
<style>
body {
    max-width: 800px;
    margin: 0 auto;
    font-size: 16px;
    font-family: system-ui;
}

pre {
    padding: 10px 20px;
    box-shadow: 0 0 2px;
    border-radius: 3px;
}

.sub-name {
    display: inline-block;
    padding: 4px 8px;
    margin-right: 8px;
    margin-bottom: 8px;
    border-radius: 5px;
    background-color: #ccc;
    font-size: 80%;
}

.children {
    display: flex;
    flex-direction: row;
}

.Constructor {
    color: #ce8500
}
.DataTypeKeyword {
    font-weight: bold;
}
.Var {
    color: #00a;
}
.NumericLiteral, .TextLiteral, .CharLiteral, .BooleanLiteral {
    color: green;
}

  .ControlKeyword      { font-weight: bold }
  .AbilityBraces       ,
  .LinkKeyword         ,
  .TypeOperator        ,
  .UseKeyword          ,
  .UsePrefix           ,
  .UseSuffix           ,
  .HashQualifier      { color: #777 }
  .DelayForceChar      { color: yellow }
  .TypeAscriptionColon { color: blue }
  .DocDelimiter        { color: green }
  .DocKeyword          { font-weight: bold }

</style>
`;

const hByHash = {};
const hierarchy = { children: {}, parent: null };
names.forEach((name) => {
    const parts = name === 'base..' ? ['base', '.'] : name.split('.');
    let last = parts.pop();

    let current = hierarchy;
    parts.forEach((part) => {
        if (!current.children[part]) {
            current.children[part] = {
                index: null,
                children: {},
                parent: current,
            };
        }
        current = current.children[part];
    });
    if (!current.children[last]) {
        current.children[last] = {
            index: null,
            children: {},
            parent: current,
        };
    }
    hByHash[byName[name].name.rhash] = current.children[last];
    current.children[last].index = byName[name];
});

const renderPage = (path, page) => {
    const namePage = path.join('.');
    return `
    <!doctype html>
    <meta charset=utf8>
    ${style}
    <title>${namePage}</title>
    <body>
        ${page.index ? renderItem(path, page.index) : ''}
        ${Object.keys(page.children)
            .sort()
            .map((key) => renderChild(path.concat([key]), page.children[key]))
            .join('\n')}
    </body>
    `;
};

const escapeHtml = (x) => x.replace(/&/g, '&amp;').replace(/</g, '&lt;');

const pathEscape = (p) =>
    p.replace(/#/g, '_HASH_').replace(/\./g, '_DOT_').replace(/:/g, '_COLON_');

const pathForHash = (hash) => {
    const item = byHash[hash];
    if (!item) {
        return null;
    }
    const h = hByHash[hash];
    if (Object.keys(h.children).length) {
        return pathEscape(hash) + '.html';
    }
    if (h.parent && h.parent.index) {
        return (
            pathEscape(h.parent.index.name.rhash) +
            '.html#' +
            pathEscape(item.name.rhash)
        );
    }
    const name = item.name.primaryName;
    const parentName = name.split('.').slice(0, -1).join('.');
    return parentName + '.html#' + pathEscape(item.name.rhash);
};

const renderBody = (body) =>
    `<code><pre>${body
        .map((i) => {
            if (i.kind === 'None') {
                return escapeHtml(i.contents);
            }
            if (i.kind.Other) {
                return `<span class="${i.kind.Other}">${escapeHtml(
                    i.contents,
                )}</span>`;
            }
            if (i.kind.WithHash) {
                return `<a class="${i.kind.WithHash.name}" href="${pathForHash(
                    i.kind.WithHash.hash,
                )}">${escapeHtml(i.contents)}</a>`;
            }
            return escapeHtml(i.contents);
        })
        .join('')}</pre></code>`;

const renderItem = (path, item) => {
    return `<h3>${path.join('.')}</h3>
    ${renderBody(item.body)}`;
};

const renderChild = (path, page) => {
    const childNames = Object.keys(page.children);
    return `<div class="child" id="${
        page.index ? pathEscape(page.index.name.rhash) : ''
    }"><h3>
        ${
            childNames.length
                ? `<a href="${
                      page.index
                          ? pathForHash(page.index.name.rhash)
                          : path.join('.') + '.html'
                  }">
            ${path.join('.')}
            </a>`
                : path.join('.')
        }
    </h3>
    ${page.index ? renderBody(page.index.body) : ''}
    ${
        childNames.length
            ? childNames
                  .map(
                      (name) => `<span class="sub-name">
                  ${`<a href="${
                      page.children[name].index
                          ? pathForHash(page.children[name].index.name.rhash)
                          : path.concat([name]).join('.') + '.html'
                  }">${name}</a>`}
                  </span>`,
                  )
                  .join(' ')
            : ''
    }
    </div>`;
};

const mkdirp = (d) => {
    if (!fs.existsSync(d)) {
        mkdirp(path.dirname(d));
        fs.mkdirSync(d);
    }
};

const traverse = (dest, trail, page) => {
    const fileName = path.join(
        dest,
        page.index
            ? pathForHash(page.index.name.rhash)
            : trail.join('.') + '.html',
    );
    mkdirp(path.dirname(fileName));
    fs.writeFileSync(fileName, renderPage(trail, page), 'utf8');
    Object.keys(page.children).forEach((key) => {
        if (Object.keys(page.children[key].children).length) {
            traverse(dest, trail.concat([key]), page.children[key]);
        }
    });
};

traverse(destDir, [], hierarchy);
