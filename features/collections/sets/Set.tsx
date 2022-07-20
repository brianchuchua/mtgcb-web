import LinearProgress from '@material-ui/core/LinearProgress';
import Typography from '@material-ui/core/Typography';
import Skeleton from '@material-ui/lab/Skeleton';
import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { ResponsiveContainer } from '../../../components/layout/ResponsiveContainer';
import Link from '../../../components/Link';
import { useGetSetBySlugQuery, useGetSetSummaryLegacyQuery } from '../../../network/services/mtgcbApi';
import { RootState } from '../../../redux/rootReducer';
import { useLocalStorage } from '../../../util';
import { SetIcon } from '../../browse/SetBox';
import { formatter } from '../../browse/util/formatPrice';
import { ConnectedCollectionCardGallery } from '../ConnectedCollectionCardGallery';
import { ConnectedCollectionCardTable } from '../ConnectedCollectionCardTable';
import { setFormVisibility } from './setCollectionSlice';

interface SetProps {
  setSlug: string;
  userId?: string;
}

export const Set: React.FC<SetProps> = ({ setSlug, userId }) => {
  const { searchQuery, oracleTextQuery, viewSubject, viewMode, priceType } = useSelector((state: RootState) => state.setCollection);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useLocalStorage('numberOfCardsPerPage', 50);
  const [page, setPage] = useState(1);

  const { data: setData, isLoading: isSetLoading, isFetching: isSetFetching, error: setError } = useGetSetBySlugQuery(
    { slug: setSlug },
    { skip: setSlug == null }
  );

  const setId = setData?.data?.allSets?.[0]?.id;
  const {
    data: setSummaryData,
    isLoading: isSetSummaryLoading,
    isFetching: isSetSummaryFetching,
    error: setSummaryError,
  } = useGetSetSummaryLegacyQuery(
    {
      setId: setData?.data?.allSets?.[0]?.id,
      userId,
    },
    { skip: setId == null || userId == null }
  );

  const set = setData?.data?.allSets?.[0];
  const setSummary = setSummaryData?.data?.setSummaryLegacy;
  const username = setSummary?.username ?? '';

  const isLoading = isSetLoading || isSetSummaryLoading;
  const isFetching = isSetFetching || isSetSummaryFetching;

  const collectionDetails = {
    setName: set?.name,
    setCode: set?.code,
    username,
    userId,
    cardsInSet: setSummary?.cardsInSet,
    totalCardsCollectedInSet: setSummary?.totalCardsCollectedInSet,
    uniquePrintingsCollectedInSet: setSummary?.uniquePrintingsCollectedInSet,
    percentageCollected: setSummary?.percentageCollected,
    totalValue: setSummary?.totalValue,
  };

  // TODO: Add buy links here and come up with a good interface, similar to how Scryfall does card pages perhaps

  return (
    <ResponsiveContainer maxWidth="xl">
      <>
        {set && (
          <>
            <SetCollectionDetails
              collectionDetails={collectionDetails}
              priceType={priceType}
              isLoading={isLoading}
              isFetching={isFetching}
            />
            {viewSubject === 'cards' && viewMode === 'grid' && (
              <ConnectedCollectionCardGallery
                userId={userId}
                setId={setData?.data?.allSets?.[0]?.id}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
              />
            )}
            {viewSubject === 'cards' && viewMode === 'table' && (
              <ConnectedCollectionCardTable
                userId={userId}
                setId={setData?.data?.allSets?.[0]?.id}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
              />
            )}
          </>
        )}
        {((setData?.data?.allSets && setData?.data?.allSets.length === 0) || setSummaryError) && <p>No set found</p>}
      </>
    </ResponsiveContainer>
  );
};

interface SetCollectionDetails {
  collectionDetails: {
    setName: string;
    setCode: string;
    username: string;
    userId: string;
    cardsInSet: number;
    totalCardsCollectedInSet: number;
    uniquePrintingsCollectedInSet: number;
    percentageCollected: number;
    totalValue: {
      market: number;
      low: number;
      average: number;
      high: number;
    };
  };
  priceType: string;
  isLoading: boolean;
  isFetching: boolean;
}

const SetCollectionDetails: React.FC<SetCollectionDetails> = ({ collectionDetails, priceType, isLoading, isFetching }) => {
  if (isLoading) {
    return (
      <CenteredSkeleton variant="rect" width="100%">
        <CollectionDetailsWrapper>
          <StyledCollectionDetailsTitleDesktop variant="h3">{collectionDetails.setName}</StyledCollectionDetailsTitleDesktop>
          <StyledCollectionDetailsTitleMobile variant="h5">{collectionDetails.setName}</StyledCollectionDetailsTitleMobile>
          <CollectionDetailsSubtitle variant="body1" color="textSecondary">
            <em>
              <Link href={`/collections/${collectionDetails.userId}`} color="inherit">
                (Part of {collectionDetails.username}'s collection)
              </Link>
            </em>
          </CollectionDetailsSubtitle>
          <CollectionDetailsBody>
            <SetIcon setCode={collectionDetails.setCode} />
            <Typography variant="h5" color="textSecondary" component="div">
              {collectionDetails.uniquePrintingsCollectedInSet}/{collectionDetails.cardsInSet}
            </Typography>
            <Typography variant="body2" color="textSecondary" component="div">
              <em>({collectionDetails.totalCardsCollectedInSet} total cards collected)</em>
            </Typography>
            <Typography variant="body2" color="textSecondary" component="div">
              <em>Set value: {collectionDetails?.totalValue && formatter.format(collectionDetails?.totalValue?.[priceType])}</em>
            </Typography>
            <CollectionProgressWrapper>
              <StyledLinearProgress variant="determinate" value={collectionDetails.percentageCollected ?? 0} color="secondary" />
              <LinearProgressLabel>
                <Typography variant="body2" color="textSecondary" component="div">
                  {collectionDetails.percentageCollected}% collected!
                </Typography>
              </LinearProgressLabel>
            </CollectionProgressWrapper>
          </CollectionDetailsBody>
        </CollectionDetailsWrapper>
      </CenteredSkeleton>
    );
  }

  return (
    <CollectionDetailsWrapper>
      <StyledCollectionDetailsTitleDesktop variant="h3">{collectionDetails.setName}</StyledCollectionDetailsTitleDesktop>
      <StyledCollectionDetailsTitleMobile variant="h5">{collectionDetails.setName}</StyledCollectionDetailsTitleMobile>
      <CollectionDetailsSubtitle variant="body1" color="textSecondary">
        <em>
          <Link href={`/collections/${collectionDetails.userId}`} color="inherit">
            (Part of {collectionDetails.username}'s collection)
          </Link>
        </em>
      </CollectionDetailsSubtitle>
      <CollectionDetailsBody>
        <SetIcon setCode={collectionDetails.setCode} />
        <Typography variant="h5" color="textSecondary" component="div">
          {collectionDetails.uniquePrintingsCollectedInSet}/{collectionDetails.cardsInSet}
        </Typography>
        <Typography variant="body2" color="textSecondary" component="div">
          <em>({collectionDetails.totalCardsCollectedInSet} total cards collected)</em>
        </Typography>
        <Typography variant="body2" color="textSecondary" component="div">
          <em>Set value: {collectionDetails?.totalValue && formatter.format(collectionDetails?.totalValue?.[priceType])}</em>
        </Typography>
        <CollectionProgressWrapper>
          <StyledLinearProgress variant="determinate" value={collectionDetails.percentageCollected ?? 0} color="secondary" />
          <LinearProgressLabel>
            <Typography variant="body2" color="textSecondary" component="div">
              {collectionDetails.percentageCollected}% collected!
            </Typography>
          </LinearProgressLabel>
        </CollectionProgressWrapper>
      </CollectionDetailsBody>
    </CollectionDetailsWrapper>
  );
};

const CollectionDetailsWrapper = styled.div(() => ({
  width: '100%',
}));

const CollectionDetailsTitle = styled(Typography)({
  textAlign: 'center',
});

const CollectionDetailsSubtitle = styled(Typography)({
  textAlign: 'center',
});

const CollectionDetailsBody = styled.div({
  textAlign: 'center',
});

const CollectionProgressWrapper = styled.div({
  position: 'relative',
});

const StyledLinearProgress = styled(LinearProgress)({
  height: '25px',
  width: '100%',
  margin: '10px auto 10px',
  textAlign: 'center',
});

const LinearProgressLabel = styled.div({
  position: 'absolute',
  top: '3px',
  textAlign: 'center',
  margin: '0 auto',
  width: '100%',
});

const StyledCollectionDetailsTitleMobile = styled(CollectionDetailsTitle)(({ theme }) => ({
  [theme.breakpoints.up('lg')]: {
    display: 'none',
  },
}));

const StyledCollectionDetailsTitleDesktop = styled(CollectionDetailsTitle)(({ theme }) => ({
  [theme.breakpoints.down('md')]: {
    display: 'none',
  },
}));

const CenteredSkeleton = styled(Skeleton)({
  margin: '0 auto',
});
