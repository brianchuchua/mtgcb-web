import Box from '@material-ui/core/Box';
import Button from '@material-ui/core/Button';
import CircularProgress from '@material-ui/core/CircularProgress';
import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';
import { useState } from 'react';
import styled from 'styled-components';
import Link from '../../components/Link';
import { tcgplayerMassImport, tcgplayerMassImportForUserLegacy } from '../../network/features/browse';
import { PriceTypes } from './browseSlice';
import { formatter } from './util/formatPrice';
import titleCase from './util/titleCase';

const SetBox: React.FC<SetBoxProps> = ({ set, costsToPurchaseInSet, priceType, isComplete = false, userId = null }) => (
  <SetBoxWrapper variant="outlined">
    <SetName>
      <Link href={userId ? `/collections/${userId}/sets/${set.slug}` : `/browse/sets/${set.slug}`}>
        {set.name} ({set.code})
      </Link>
    </SetName>
    <Typography variant="body2" color="textSecondary" component="div">
      {set.category} Set
      {set.setType ? ` - ${titleCase(set.setType)}` : ''}
    </Typography>
    <Typography variant="body2" color="textSecondary" component="div">
      {set.releasedAt?.slice(0, 10)}
    </Typography>
    {costsToPurchaseInSet && userId ? (
      <>
        <SetIconWithRadialProgress percentage={costsToPurchaseInSet.percentageCollected} setCode={set.code} />
        <SetStatisticsInCollection
          cardCount={costsToPurchaseInSet?.cardsInSet}
          totalCardsCollectedInSet={costsToPurchaseInSet?.totalCardsCollectedInSet}
          uniquePrintingsCollectedInSet={costsToPurchaseInSet?.uniquePrintingsCollectedInSet}
        />
        <Typography variant="body2" color="textSecondary" component="div">
          <em>Current set value: {formatter.format(costsToPurchaseInSet[priceType].totalValue)}</em>
        </Typography>
      </>
    ) : (
      <>
        <SetIcon setCode={set.code} />
        <SetStatistics cardCount={set.cardCount} />
      </>
    )}
    {costsToPurchaseInSet && (
      <div style={{ marginTop: '10px' }}>
        <Typography variant="body2" color="textSecondary" component="div">
          {userId ? 'Costs to complete:' : 'Costs to purchase:'}
        </Typography>

        <table style={{ display: 'inline-block', textAlign: 'center' }}>
          <tbody>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x all cards: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachCard)}</em>
                </Typography>
              </td>
              <td style={{ minWidth: '145px' }}>
                <BuyThisButton setId={set.id} count={1} countType="all" userId={userId} />
                <BuyThisButton setId={set.id} count={4} countType="all" userId={userId} />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x mythics: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachMythic)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="mythic" userId={userId} />
                <BuyThisButton setId={set.id} count={4} countType="mythic" userId={userId} />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x rares: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachRare)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="rare" userId={userId} />
                <BuyThisButton setId={set.id} count={4} countType="rare" userId={userId} />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x uncommons: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachUncommon)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="uncommon" userId={userId} />
                <BuyThisButton setId={set.id} count={4} countType="uncommon" userId={userId} />
              </td>
            </tr>
            <tr>
              <td style={{ textAlign: 'right' }}>
                <Typography variant="body2" color="textSecondary">
                  <em>1x commons: {formatter.format(costsToPurchaseInSet[priceType].oneOfEachCommon)}</em>
                </Typography>
              </td>
              <td>
                <BuyThisButton setId={set.id} count={1} countType="common" userId={userId} />
                <BuyThisButton setId={set.id} count={4} countType="common" userId={userId} />
              </td>
            </tr>
            {set.sealedProductUrl ? (
              <tr>
                <td colSpan={3}>
                  <Button
                    style={{ textTransform: 'capitalize', marginTop: '3px' }}
                    variant="outlined"
                    size="small"
                    href={`${set.sealedProductUrl}&partner=CTNBLDR&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR&ProductTypeName=Sealed`}
                    target="_blank"
                    fullWidth
                  >
                    Buy this set sealed
                  </Button>
                </td>
              </tr>
            ) : null}
            {set.isDraftable ? (
              <tr>
                <td colSpan={3}>
                  <BuyThisButton
                    setId={set.id}
                    count={1}
                    countType="draftcube"
                    price={formatter.format(costsToPurchaseInSet[priceType].draftCube)}
                    userId={userId}
                  />
                </td>
              </tr>
            ) : null}
          </tbody>
        </table>
      </div>
    )}
  </SetBoxWrapper>
);

type CountType = 'all' | 'mythic' | 'rare' | 'uncommon' | 'common' | 'draftcube';

interface BuyThisButtonProps {
  setId: string;
  userId?: string;
  count: number;
  countType: CountType;
  price?: string;
}

const BuyThisButton = ({ setId, count, countType, price, userId = null }: BuyThisButtonProps) => {
  const [tcgplayerMassImportString, setTcgplayerMassImportString] = useState('');

  return (
    <form
      method="post"
      action="https://store.tcgplayer.com/massentry?partner=CTNBLDR"
      target="_blank"
      id={`tcgplayer-mass-import-form-${setId}-${count}-${countType}`}
      onSubmit={(e) => handleBuyThisSubmit(e, setId, count, countType, setTcgplayerMassImportString, userId)}
      style={{ display: 'inline-block', width: countType === 'draftcube' ? '100%' : 'auto' }}
    >
      <input type="hidden" name="partner" value="CTNBLDR" />
      <input type="hidden" name="c" value={tcgplayerMassImportString} />
      <Button
        style={{
          textTransform: 'capitalize',
          marginLeft: countType === 'draftcube' ? '0px' : '5px',
          marginTop: countType === 'draftcube' ? '3px' : '0px',
        }}
        variant="outlined"
        size="small"
        type="submit"
        fullWidth={countType === 'draftcube'}
      >
        {countType === 'draftcube' ? `Buy a draft cube for ${price}` : `Buy ${count}x`}
      </Button>
    </form>
  );
};

const handleBuyThisSubmit = async (e, setId, count, countType, setTcgplayerMassImportString, userId = null) => {
  e.preventDefault();
  const options: any = { setId: parseInt(setId, 10) }; // eslint-disable-line @typescript-eslint/no-explicit-any

  if (countType === 'all') {
    options.allCount = count;
  } else if (countType === 'draftcube') {
    options.draftCubeCount = count;
  } else if (countType === 'mythic') {
    options.mythicCount = count;
  } else if (countType === 'rare') {
    options.rareCount = count;
  } else if (countType === 'uncommon') {
    options.uncommonCount = count;
  } else if (countType === 'common') {
    options.commonCount = count;
  }

  if (userId) {
    options.userId = userId;
  }

  const tcgplayerMassImportString = userId
    ? (await tcgplayerMassImportForUserLegacy(options))?.data?.data?.tcgplayerMassImportForUserLegacy?.tcgplayerMassImport
    : (await tcgplayerMassImport(options))?.data?.data?.tcgplayerMassImport?.tcgplayerMassImport;
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

export interface Set {
  id: string;
  name: string;
  code: string;
  category: string;
  setType: string;
  cardCount: number;
  releasedAt: string;
  sealedProductUrl: string;
  isDraftable: boolean;
  slug: string;
  costsToPurchase?: SetSummary;
}

interface SetBoxProps {
  set: Set;
  costsToPurchaseInSet: SetSummary;
  priceType: PriceTypes;
  isComplete?: boolean;
  userId?: string;
}

export interface SetSummary {
  setId: number;
  cardsInSet?: number;
  totalCardsCollectedInSet?: number;
  uniquePrintingsCollectedInSet?: number;
  percentageCollected?: number;
  market: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
    fourOfEachCard?: number;
    fourOfEachMythic?: number;
    fourOfEachRare?: number;
    fourOfEachUncommon?: number;
    fourOfEachCommon?: number;
    draftCube?: number;
  };
  low: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
    fourOfEachCard?: number;
    fourOfEachMythic?: number;
    fourOfEachRare?: number;
    fourOfEachUncommon?: number;
    fourOfEachCommon?: number;
    draftCube?: number;
  };
  average: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
    fourOfEachCard?: number;
    fourOfEachMythic?: number;
    fourOfEachRare?: number;
    fourOfEachUncommon?: number;
    fourOfEachCommon?: number;
    draftCube?: number;
  };
  high: {
    oneOfEachCard: number;
    oneOfEachMythic: number;
    oneOfEachRare: number;
    oneOfEachUncommon: number;
    oneOfEachCommon: number;
    fourOfEachCard?: number;
    fourOfEachMythic?: number;
    fourOfEachRare?: number;
    fourOfEachUncommon?: number;
    fourOfEachCommon?: number;
    draftCube?: number;
  };
}

interface SetStatisticsProps {
  cardCount?: number;
}

const SetStatistics: React.FC<SetStatisticsProps> = ({ cardCount }) => (
  <Typography variant="body2" color="textSecondary" component="div">
    {cardCount ? `${cardCount} cards` : ''}
  </Typography>
);

interface SetStatisticsInCollectionProps {
  cardCount?: number;
  totalCardsCollectedInSet?: number;
  uniquePrintingsCollectedInSet?: number;
}

const SetStatisticsInCollection: React.FC<SetStatisticsInCollectionProps> = ({
  cardCount,
  totalCardsCollectedInSet,
  uniquePrintingsCollectedInSet,
}) => (
  <div>
    <Typography variant="body2" color="textSecondary" component="div">
      {uniquePrintingsCollectedInSet}/{cardCount}
    </Typography>
    <Typography variant="body2" color="textSecondary" component="div">
      <em>({totalCardsCollectedInSet} total cards collected)</em>
    </Typography>
  </div>
);

interface SetIconWithRadialProgressProps {
  setCode?: string;
  percentage?: number;
}

const SetIconWithRadialProgress: React.FC<SetIconWithRadialProgressProps> = ({ percentage, setCode }) => (
  <Box position="relative" display="inline-flex" style={{ padding: '5px' }}>
    <CircularProgress variant="determinate" value={100} thickness={4.5} size={110} style={{ color: '#707070', position: 'absolute' }} />

    <Box position="relative" display="inline-flex">
      <CircularProgress
        variant="determinate"
        value={percentage}
        thickness={4.5}
        size={110}
        color="secondary"
        style={{ color: percentage < 100 ? '' : '#cd4809' }}
      />
      <Box top={0} left={0} bottom={0} right={0} position="absolute" display="flex" alignItems="center" justifyContent="center">
        <SetIcon setCode={setCode} percentage={percentage} />
      </Box>
    </Box>
  </Box>
);

interface SetIconProps {
  setCode?: string;
  percentage?: number;
  showPercentage?: boolean;
}

export const SetIcon: React.FC<SetIconProps> = ({ setCode = '', percentage = 0, showPercentage = false }) => (
  <div style={{ padding: '5px', position: 'relative' }}>
    <i
      className={`ss ss-${setCode.toLowerCase()} ss-5x ss-common ss-fw ${percentage >= 100 ? 'ss-mythic ss-grad' : ''}`}
      style={{
        WebkitTextStroke: percentage >= 100 ? '2px #000' : '',
        textShadow: percentage < 100 ? '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff' : '',
      }}
    />
    <div
      style={{
        position: 'absolute',
        fontSize: '10px',
        textAlign: 'center',
        width: '100%',
        bottom: '-17px',
        left: '1px',
        color: '#dddddd',
      }}
    >
      {showPercentage ? percentage : ''}
    </div>
  </div>
);

export default SetBox;
