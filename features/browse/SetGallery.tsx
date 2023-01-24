import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import Skeleton from '@material-ui/lab/Skeleton';
import { Dispatch, SetStateAction, useEffect, useState } from 'react';
import styled from 'styled-components';
import { useLocalStorage } from '../../util';
import { PriceTypes } from './browseSlice';
import GalleryControls from './GalleryControls';
import SetBox, { Set, SetSummary } from './SetBox';

const SetGallery: React.FC<SetGalleryProps> = ({
  sets,
  costsToPurchase,
  first,
  skip,
  page,
  totalResults,
  setSkip,
  setFirst,
  setPage,
  priceType,
  userId = null,
  isLoading,
  isFetching,
  includeSubsetsInSets = false,
}) => {
  const [setsPerRow, setSetsPerRow] = useState(4);
  const [galleryWidth, setGalleryWidth] = useState(100);
  const [showCostsToPurchase, setShowCostsToPurchase] = useLocalStorage('showCostsToPurchase', true);

  useEffect(() => {
    if (!showCostsToPurchase) {
      setSetsPerRow(5);
    } else {
      setSetsPerRow(4);
    }
  }, [showCostsToPurchase]);

  const atLeastOneSetToShow = totalResults > 0;

  const settingGroups = [
    {
      label: 'Show Fields',
      type: 'toggleFilters',
      settings: [
        {
          key: 'costsToPurchase',
          label: 'Costs to Purchase',
          isVisible: showCostsToPurchase,
          setVisibility: setShowCostsToPurchase,
        },
      ],
    },
  ];

  if (isLoading || isFetching) {
    const boxHeightCollectionView = 500;
    const boxHeightCollectorViewCollapsed = 254;
    const boxHeightBrowseView = 422;
    const boxHeightBrowseViewCollapsed = 172;

    const isCollectionView = userId != null;
    const isBrowseView = userId == null;
    const isCollapsed = !showCostsToPurchase;

    const determineSkeletonHeight = () => {
      let calculatedHeight = 0;
      if (isCollectionView && isCollapsed) {
        calculatedHeight = boxHeightCollectorViewCollapsed;
      } else if (isCollectionView && !isCollapsed) {
        calculatedHeight = boxHeightCollectionView;
      } else if (isBrowseView && isCollapsed) {
        calculatedHeight = boxHeightBrowseViewCollapsed;
      } else if (isBrowseView && !isCollapsed) {
        calculatedHeight = boxHeightBrowseView;
      }
      return calculatedHeight;
    };

    const skeletonHeight = determineSkeletonHeight();

    if (typeof window === 'undefined') {
      return null;
    }

    return (
      <>
        <GalleryControls
          items={sets}
          first={first}
          page={page}
          setCardsPerRow={setSetsPerRow}
          setFirst={setFirst}
          setGalleryWidth={setGalleryWidth}
          setPage={setPage}
          setSkip={setSkip}
          skip={skip}
          totalResults={totalResults}
          cardsPerRow={setsPerRow}
          galleryType="sets"
          isLoading={isLoading}
          isFetching={isFetching}
          settingGroups={settingGroups}
        />{' '}
        <SetGalleryWrapper setsPerRow={setsPerRow} galleryWidth={galleryWidth} isCompact={!showCostsToPurchase}>
          {Array.from({ length: first }, (_, i) => (
            <Skeleton key={i} variant="rect" height={skeletonHeight} animation="wave" />
          ))}
        </SetGalleryWrapper>
      </>
    );
  }
  return atLeastOneSetToShow ? (
    <>
      <GalleryControls
        items={sets}
        first={first}
        page={page}
        setCardsPerRow={setSetsPerRow}
        setFirst={setFirst}
        setGalleryWidth={setGalleryWidth}
        setPage={setPage}
        setSkip={setSkip}
        skip={skip}
        totalResults={totalResults}
        cardsPerRow={setsPerRow}
        isFetching={isFetching}
        galleryType="sets"
        settingGroups={settingGroups}
      />

      <SetGalleryWrapper setsPerRow={setsPerRow} galleryWidth={galleryWidth} isCompact={!showCostsToPurchase}>
        {sets &&
          sets.map((set) => {
            const costsToPurchaseInSet = costsToPurchase?.find((costs) => Number(costs.setId) === Number(set.id));
            return (
              <SetBox
                key={`set-box-${set.id}`}
                set={set}
                costsToPurchaseInSet={costsToPurchaseInSet}
                priceType={priceType}
                userId={userId}
                showCostsToPurchase={showCostsToPurchase}
                includeSubsetsInSets={includeSubsetsInSets}
              />
            );
          })}
      </SetGalleryWrapper>

      <div style={{ marginTop: '20px' }}>
        <GalleryControls
          items={sets}
          first={first}
          page={page}
          setCardsPerRow={setSetsPerRow}
          setFirst={setFirst}
          setGalleryWidth={setGalleryWidth}
          setPage={setPage}
          setSkip={setSkip}
          skip={skip}
          totalResults={totalResults}
          cardsPerRow={setsPerRow}
          isFetching={isFetching}
          galleryType="sets"
          isOnBottom
          settingGroups={settingGroups}
        />
      </div>
    </>
  ) : (
    <Grid container alignItems="center" justify="center">
      <Grid item>
        <Typography variant="h6">No results found -- try another search!</Typography>
      </Grid>
    </Grid>
  );
};

interface SetGalleryWrapperProps {
  setsPerRow: number;
  galleryWidth: number;
  isCompact: boolean;
}

const SetGalleryWrapper = styled.div<SetGalleryWrapperProps>(({ setsPerRow = 4, galleryWidth = 100, isCompact = false, theme }) => ({
  display: 'grid',
  [theme.breakpoints.up('xs')]: {
    gridTemplateColumns: `repeat(1, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('sm')]: {
    gridTemplateColumns: `repeat(2, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('md')]: {
    gridTemplateColumns: `repeat(${isCompact ? 3 : 2}, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('lg')]: {
    gridTemplateColumns: `repeat(${isCompact ? 4 : 3}, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('xl')]: {
    gridTemplateColumns: `repeat(${isCompact ? 5 : 4}, minmax(0, 1fr))`,
  },

  gridTemplateRows: 'repeat(1, 1fr)',
  gap: `10px 10px`,
  width: `${galleryWidth}%`,
  margin: '0 auto',
}));

interface SetGalleryProps {
  sets: Set[];
  costsToPurchase: SetSummary[];
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
  priceType: PriceTypes;
  userId?: string;
  isLoading: boolean;
  isFetching: boolean;
  includeSubsetsInSets?: boolean;
}

export default SetGallery;
