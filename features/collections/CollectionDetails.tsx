import LinearProgress from '@material-ui/core/LinearProgress';
import Typography from '@material-ui/core/Typography';
import Skeleton from '@material-ui/lab/Skeleton';
import styled from 'styled-components';
import { formatter } from '../browse/util/formatPrice';

export const CollectionDetails: React.FC<CollectionDetailsProps> = ({ collectionDetails, isLoading }) => {
  if (isLoading) {
    return (
      <CollectionDetailsWrapper>
        <CenteredSkeleton variant="text" width={400}>
          <StyledCollectionDetailsTitleDesktop variant="h3">{collectionDetails.username}'s collection</StyledCollectionDetailsTitleDesktop>
        </CenteredSkeleton>
        <CenteredSkeleton variant="text" width={300}>
          <StyledCollectionDetailsTitleMobile variant="h5">{collectionDetails.username}'s Collection</StyledCollectionDetailsTitleMobile>
        </CenteredSkeleton>

        <CollectionDetailsBody>
          <CenteredSkeleton variant="rect" width={200}>
            <StyledCollectionDetailsTitleDesktop variant="h4" color="textSecondary">
              {collectionDetails.uniquePrintingsCollected}/{collectionDetails.numberOfCardsInMagic}
            </StyledCollectionDetailsTitleDesktop>
            <StyledCollectionDetailsTitleMobile variant="h6" color="textSecondary">
              {collectionDetails.uniquePrintingsCollected}/{collectionDetails.numberOfCardsInMagic}
            </StyledCollectionDetailsTitleMobile>
            <Typography variant="body2" color="textSecondary" component="div">
              <em>({collectionDetails.totalCardsCollected} total cards collected)</em>
            </Typography>
            <Typography variant="body2" color="textSecondary" component="div">
              <em>Collection value: {formatter.format(collectionDetails.totalValue)}</em>
            </Typography>
          </CenteredSkeleton>
          <CenteredSkeleton variant="rect" width="100%">
            <CollectionProgressWrapper>
              <StyledLinearProgress variant="determinate" value={collectionDetails.percentageCollected ?? 0} color="secondary" />
              <LinearProgressLabel>
                <Typography variant="body2" color="textSecondary" component="div">
                  {collectionDetails.percentageCollected}% collected!
                </Typography>
              </LinearProgressLabel>
            </CollectionProgressWrapper>
          </CenteredSkeleton>
        </CollectionDetailsBody>
      </CollectionDetailsWrapper>
    );
  }

  return (
    <CollectionDetailsWrapper>
      <StyledCollectionDetailsTitleDesktop variant="h3">{collectionDetails.username}'s Collection</StyledCollectionDetailsTitleDesktop>
      <StyledCollectionDetailsTitleMobile variant="h5">{collectionDetails.username}'s Collection</StyledCollectionDetailsTitleMobile>
      <CollectionDetailsBody>
        <StyledCollectionDetailsTitleDesktop variant="h4" color="textSecondary">
          {collectionDetails.uniquePrintingsCollected}/{collectionDetails.numberOfCardsInMagic}
        </StyledCollectionDetailsTitleDesktop>
        <StyledCollectionDetailsTitleMobile variant="h6" color="textSecondary">
          {collectionDetails.uniquePrintingsCollected}/{collectionDetails.numberOfCardsInMagic}
        </StyledCollectionDetailsTitleMobile>
        <Typography variant="body2" color="textSecondary" component="div">
          <em>({collectionDetails.totalCardsCollected} total cards collected)</em>
        </Typography>
        <Typography variant="body2" color="textSecondary" component="div">
          <em>Collection value: {formatter.format(collectionDetails.totalValue)}</em>
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

export interface CollectionDetailsProps {
  collectionDetails: {
    username: string;
    numberOfCardsInMagic: number;
    totalCardsCollected: number;
    uniquePrintingsCollected: number;
    percentageCollected: number;
    totalValue: number;
  };
  isLoading: boolean;
}

const CollectionDetailsWrapper = styled.div(() => ({
  width: '100%',
}));

const CollectionDetailsTitle = styled(Typography)({
  textAlign: 'center',
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

const CenteredSkeleton = styled(Skeleton)({
  margin: '0 auto',
});
