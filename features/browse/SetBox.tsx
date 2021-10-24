import Button from '@material-ui/core/Button';
import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';
import { useState } from 'react';
import styled from 'styled-components';
import { tcgplayerMassImport } from '../../network/features/browse';
import { PriceTypes } from './browseSlice';
import { formatter } from './util/formatPrice';
import titleCase from './util/titleCase';

// TODO: Make a component for the set icon
const SetBox: React.FC<SetBoxProps> = ({ set, costsToPurchaseInSet, priceType, isComplete = false }) => (
  <SetBoxWrapper variant="outlined">
    <SetName>
      {set.name} ({set.code})
    </SetName>
    <Typography variant="body2" color="textSecondary" component="div">
      {set.releasedAt?.slice(0, 10)}
    </Typography>
    <div style={{ padding: '5px' }}>
      <i
        className={`ss ss-${set.code.toLowerCase()} ss-5x ss-common ss-fw`}
        style={{
          // WebkitTextStroke: '1px #fff', // TODO: Use this style for a complete set so I can support ss-mythic ss-grad
          textShadow: '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff',
        }}
      />
    </div>
    <Typography variant="body2" color="textSecondary" component="div">
      {set.cardCount ? `${set.cardCount} cards` : ''}
    </Typography>
    <Typography variant="body2" color="textSecondary" component="div">
      {set.category} Set
      {set.setType ? ` - ${titleCase(set.setType)}` : ''}
    </Typography>
    {costsToPurchaseInSet && (
      <div style={{ marginTop: '10px' }}>
        <table style={{ display: 'inline-block', textAlign: 'center' }}>
          <tbody>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x all cards: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachCard)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="all" />
                <BuyThisButton setId={set.id} count={4} countType="all" />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x mythics: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachMythic)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="mythic" />
                <BuyThisButton setId={set.id} count={4} countType="mythic" />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x rares: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachRare)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="rare" />
                <BuyThisButton setId={set.id} count={4} countType="rare" />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x uncommons: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachUncommon)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="uncommon" />
                <BuyThisButton setId={set.id} count={4} countType="uncommon" />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x commons: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachCommon)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="common" />
                <BuyThisButton setId={set.id} count={4} countType="common" />
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    )}
  </SetBoxWrapper>
);

type CountType = 'all' | 'mythic' | 'rare' | 'uncommon' | 'common';

interface BuyThisButtonProps {
  setId: string;
  count: number;
  countType: CountType;
}

const BuyThisButton = ({ setId, count, countType }: BuyThisButtonProps) => {
  const [tcgplayerMassImportString, setTcgplayerMassImportString] = useState('');

  return (
    <form
      method="post"
      action="https://store.tcgplayer.com/massentry?partner=CTNBLDR"
      target="_blank"
      id={`tcgplayer-mass-import-form-${setId}-${count}-${countType}`}
      onSubmit={(e) => handleBuyThisSubmit(e, setId, count, countType, setTcgplayerMassImportString)}
      style={{ display: 'inline-block' }}
    >
      <input type="hidden" name="partner" value="CTNBLDR" />
      <input type="hidden" name="c" value={tcgplayerMassImportString} />
      <Button style={{ textTransform: 'capitalize', marginLeft: '5px' }} variant="outlined" size="small" type="submit">
        Buy {count}x
      </Button>
    </form>
  );
};

const handleBuyThisSubmit = async (e, setId, count, countType, setTcgplayerMassImportString) => {
  e.preventDefault();
  const options: any = { setId: parseInt(setId, 10) }; // eslint-disable-line @typescript-eslint/no-explicit-any

  if (countType === 'all') {
    options.allCount = count;
  } else if (countType === 'mythic') {
    options.mythicCount = count;
  } else if (countType === 'rare') {
    options.rareCount = count;
  } else if (countType === 'uncommon') {
    options.uncommonCount = count;
  } else if (countType === 'common') {
    options.commonCount = count;
  }

  const tcgplayerMassImportString = (await tcgplayerMassImport(options))?.data?.data?.tcgplayerMassImport?.tcgplayerMassImport;
  console.log(tcgplayerMassImportString);
  setTcgplayerMassImportString(tcgplayerMassImportString);
  const buyThisForm: any = document.getElementById(`tcgplayer-mass-import-form-${setId}-${count}-${countType}`); // eslint-disable-line @typescript-eslint/no-explicit-any
  buyThisForm.submit();
};

const SetBoxWrapper = styled(Paper)({
  textAlign: 'center',
  padding: '5px',
});

const SetName = styled.div(({ theme }) => ({
  fontWeight: 'bold',
  color: theme.palette.primary.main,
}));

const SetReleaseDate = styled.div({});
export interface Set {
  id: string;
  name: string;
  code: string;
  category: string;
  setType: string;
  cardCount: number;
  releasedAt: string;
}

interface SetBoxProps {
  set: Set;
  costsToPurchaseInSet: CostToPurchase;
  priceType: PriceTypes;
  isComplete?: boolean;
}

export interface CostToPurchase {
  setId: number;
  market: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
  };
  low: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
  };
  average: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
  };
  high: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
  };
}

export default SetBox;
