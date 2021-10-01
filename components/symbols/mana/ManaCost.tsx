import { manaSymbolRankingsAscending, manaSymbolRankingsDescending } from './data/manaSymbolRankings';
import ManaSymbol from './ManaSymbol';

const ManaCost: React.FC<{ manaCost: string }> = ({ manaCost }) => {
  const tokenizedManaCost = tokenizeManaCost(manaCost);

  return (
    <>
      {tokenizedManaCost?.length > 0 &&
        tokenizedManaCost.map((token, index) => {
          const half = token.includes('h');
          const cleanedToken = token.replace('h', '');
          if (cleanedToken === 'double_faced') {
            return ' // ';
          }
          return <ManaSymbol key={`tokenized-mana-cost-browse-table-${index}`} symbol={cleanedToken} size={1} margin={1} half={half} />;
        })}
    </>
  );
};

// {1}{W}{W} --> ['1', 'W', 'W']
export const tokenizeManaCost = (manaCost: string): string[] => {
  if (manaCost == null || manaCost === '') {
    return [];
  }

  const splitManaCost = manaCost.replace(/\/\//g, '//{DOUBLE_FACED}//').split('//');
  const tokenizedManaCosts = splitManaCost?.map((manaCostToken) => {
    const manaCostCommaSeparated = manaCostToken
      .trim()
      .replace(/}/g, '},')
      .replace(/{/g, '')
      .replace(/}/g, '')
      .replace(/\//g, '')
      .slice(0, -1);
    return manaCostCommaSeparated.toLowerCase().split(',');
  });

  return tokenizedManaCosts.flat();
};

export const sortByManaSymbols = (leftManaCost: string, rightManaCost: string, desc: boolean) => {
  const leftManaCostTokens = tokenizeManaCost(leftManaCost);
  const rightManaCostTokens = tokenizeManaCost(rightManaCost);
  if (!leftManaCostTokens || !rightManaCostTokens) {
    return 0;
  }
  let leftManaCostIndex = 0;
  let rightManaCostIndex = 0;
  while (leftManaCostIndex < leftManaCostTokens.length && rightManaCostIndex < rightManaCostTokens.length) {
    const leftManaCostToken = leftManaCostTokens[leftManaCostIndex];
    const rightManaCostToken = rightManaCostTokens[rightManaCostIndex];
    if (leftManaCostToken === rightManaCostToken) {
      leftManaCostIndex += 1;
      rightManaCostIndex += 1;
      continue; // eslint-disable-line no-continue
    }
    const rankingToUse = desc ? manaSymbolRankingsDescending : manaSymbolRankingsAscending;
    const leftManaSymbolRank = rankingToUse[leftManaCostToken] ?? Infinity;
    const rightManaSymbolRank = rankingToUse[rightManaCostToken] ?? Infinity;
    if (leftManaSymbolRank - rightManaSymbolRank !== 0) {
      return leftManaSymbolRank - rightManaSymbolRank;
    }
    if (leftManaCostToken.length > 1 && rightManaCostToken.length > 1) {
      const leftManaCostNumber = parseInt(leftManaCostToken, 10);
      const rightManaCostNumber = parseInt(rightManaCostToken, 10);
      if (leftManaCostNumber - rightManaCostNumber !== 0) {
        return leftManaCostNumber - rightManaCostNumber;
      }
    }
    return leftManaCostToken.localeCompare(rightManaCostToken, 'en', { numeric: true, sensitivity: 'base' });
  }
  if (leftManaCostIndex < leftManaCostTokens.length) {
    return 1;
  }
  if (rightManaCostIndex < rightManaCostTokens.length) {
    return -1;
  }
  return 0;
};

export default ManaCost;
