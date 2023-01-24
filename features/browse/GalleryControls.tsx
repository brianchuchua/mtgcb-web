import Grid from '@material-ui/core/Grid';
import Slider from '@material-ui/core/Slider';
import Typography from '@material-ui/core/Typography';
import Skeleton from '@material-ui/lab/Skeleton';
import { Dispatch, SetStateAction, useEffect, useState } from 'react';
import { forceCheck as forceImagesToCheckIfTheyShouldLoad } from 'react-lazyload';
import styled from 'styled-components';
import Pagination from '../../components/Pagination';
import SettingsPanel, { SettingGroup } from '../../components/SettingsPanel';
import breakpoints from '../../themes/breakpoints';
import { useWindowDimensions } from '../../util';
import { JumpToSelect, NumberOfItemsSelect } from './forms';

type GalleryTypes = 'cards' | 'sets';

interface GalleryControlsProps {
  items: any[]; // eslint-disable-line @typescript-eslint/no-explicit-any
  cardsPerRow?: number;
  page: number;
  first: number;
  skip: number;
  galleryWidth?: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  setCardsPerRow?: Dispatch<SetStateAction<number>>;
  setGalleryWidth?: Dispatch<SetStateAction<number>>;
  totalResults: number;
  settingGroups?: SettingGroup[];
  galleryType?: GalleryTypes;
  isLoading?: boolean;
  isFetching: boolean;
  isOnBottom?: boolean;
  goToOptions?: { label: string; value: string }[];
}

const GalleryControls: React.FC<GalleryControlsProps> = ({
  items,
  first,
  skip,
  page,
  totalResults,
  galleryWidth,
  cardsPerRow,
  setSkip,
  setFirst,
  setPage,
  setCardsPerRow,
  setGalleryWidth,
  galleryType,
  settingGroups = [],
  isLoading,
  isFetching,
  isOnBottom = false,
  goToOptions = [],
}) => {
  const startOfRange = 1 + skip;
  const numberOfCards = items?.length ?? 0;
  const endOfRange = numberOfCards < first ? skip + numberOfCards : skip + first;

  const atLeastOneItemToShow = totalResults > 0;

  const typeLabel = galleryType === 'cards' ? 'Cards' : 'Sets';

  useEffect(() => {
    forceImagesToCheckIfTheyShouldLoad();
  }, [items, first, skip, page, totalResults]);

  const { width } = useWindowDimensions();

  useEffect(() => {
    if (width < breakpoints.sm) {
      handleCardsPerRowChange(1, setCardsPerRow);
    } else {
      handleCardsPerRowChange(cardsPerRow, setCardsPerRow);
    }
  }, [width]);

  if (isLoading) {
    return (
      <div style={{ marginTop: '10px', marginBottom: '10px' }}>
        <Skeleton variant="rect" width="100%" height={galleryType === 'sets' ? '54px' : '108px'} />
      </div>
    );
  }

  return (
    atLeastOneItemToShow && (
      <>
        <GalleryPaginationDesktop
          settingGroups={settingGroups}
          startOfRange={startOfRange}
          endOfRange={endOfRange}
          totalResults={totalResults}
          typeLabel={typeLabel}
          galleryType={galleryType}
          page={page}
          first={first}
          setFirst={setFirst}
          setPage={setPage}
          setSkip={setSkip}
          isOnBottom={isOnBottom}
          isFetching={isFetching}
          goToOptions={goToOptions}
        />
        <GalleryPaginationMobile
          settingGroups={settingGroups}
          startOfRange={startOfRange}
          endOfRange={endOfRange}
          totalResults={totalResults}
          typeLabel={typeLabel}
          galleryType={galleryType}
          page={page}
          first={first}
          setFirst={setFirst}
          setPage={setPage}
          setSkip={setSkip}
          isOnBottom={isOnBottom}
          isFetching={isFetching}
          goToOptions={goToOptions}
        />
        {setGalleryWidth && setCardsPerRow && width >= breakpoints.sm && galleryType === 'cards' && !isOnBottom && (
          <Grid container item sm={12} spacing={2} justify="space-between" alignItems="center">
            <Grid item sm={6}>
              <Typography id="card-size-slider">Card size</Typography>
              <Slider
                defaultValue={galleryWidth}
                aria-labelledby="card-size-slider"
                valueLabelDisplay="auto"
                step={5}
                min={0}
                max={100}
                onChange={(e, value: number) => handleGalleryWidthChange(value, setGalleryWidth)}
              />
            </Grid>
            <Grid item sm={6}>
              <Typography id="cards-per-row-slider">Cards per row</Typography>
              <Slider
                defaultValue={cardsPerRow}
                aria-labelledby="cards-per-row-slider"
                valueLabelDisplay="auto"
                step={1}
                marks
                min={1}
                max={9}
                onChange={(e, value: number) => handleCardsPerRowChange(value, setCardsPerRow)}
              />
            </Grid>
          </Grid>
        )}
      </>
    )
  );
};

const handleGalleryWidthChange = (value, setGalleryWidth) => {
  setGalleryWidth(value);
  forceImagesToCheckIfTheyShouldLoad();
  triggerBrowserResizeEvent();
};

const handleCardsPerRowChange = (value, setCardsPerRow) => {
  if (setCardsPerRow) {
    setCardsPerRow(value);
  }
  forceImagesToCheckIfTheyShouldLoad();
  triggerBrowserResizeEvent();
};

const triggerBrowserResizeEvent = () => {
  const isNewerBrowser = typeof Event === 'function';
  if (isNewerBrowser) {
    window.dispatchEvent(new Event('resize'));
  } else {
    const evt = window.document.createEvent('UIEvents');
    evt.initUIEvent('resize', true, false, window, 0);
    window.dispatchEvent(evt);
  }
};

const CenteredGrid = styled(Grid)({
  textAlign: 'center',
});

const RightAlignedGrid = styled(Grid)({
  textAlign: 'right',
});

interface GalleryPaginationDesktopProps {
  settingGroups: SettingGroup[];
  startOfRange: number;
  endOfRange: number;
  totalResults: number;
  typeLabel: string;
  galleryType: GalleryTypes;
  page: number;
  first: number;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  setSkip: Dispatch<SetStateAction<number>>;
  isOnBottom?: boolean;
  isFetching: boolean;
  goToOptions?: { label: string; value: string }[];
}

const GalleryPaginationDesktop: React.FC<GalleryPaginationDesktopProps> = ({
  startOfRange,
  endOfRange,
  totalResults,
  typeLabel,
  galleryType,
  settingGroups,
  page,
  first,
  setFirst,
  setPage,
  setSkip,
  isOnBottom = false,
  isFetching = true,
  goToOptions = [],
}) => {
  const [previousStartOfRange, setPreviousStartOfRange] = useState(startOfRange);
  const [previousEndOfRange, setPreviousEndOfRange] = useState(endOfRange);
  const [previousTotalResults, setPreviousTotalResults] = useState(totalResults);

  const rangeValuesAreUpdatingViaPagination =
    totalResults === previousTotalResults && (startOfRange !== previousStartOfRange || endOfRange !== previousEndOfRange);
  const rangeValuesAreValid = startOfRange <= endOfRange && endOfRange <= totalResults && endOfRange > 0;

  useEffect(() => {
    if (!isFetching && rangeValuesAreValid) {
      setPreviousStartOfRange(startOfRange);
      setPreviousEndOfRange(endOfRange);
      setPreviousTotalResults(totalResults);
    }
  }, [isFetching, startOfRange, endOfRange, totalResults]);

  return (
    <StyledDesktopOnlyGrid
      container
      spacing={2}
      justify="center"
      alignItems="center"
      style={{ marginBottom: galleryType === 'sets' ? '10px' : '0px' }}
    >
      <Grid item lg={3}>
        {isFetching && rangeValuesAreUpdatingViaPagination && rangeValuesAreValid && (
          <Typography>{`Showing ${previousStartOfRange}-${previousEndOfRange} of ${previousTotalResults} ${typeLabel.toLowerCase()}`}</Typography>
        )}
        {isFetching && !rangeValuesAreValid && (
          <Typography
            style={{ visibility: 'hidden' }}
          >{`Showing ${previousStartOfRange}-${previousEndOfRange} of ${previousTotalResults} ${typeLabel.toLowerCase()}`}</Typography>
        )}
        {!isFetching && rangeValuesAreValid && (
          <Typography>{`Showing ${startOfRange}-${endOfRange} of ${totalResults} ${typeLabel.toLowerCase()}`}</Typography>
        )}
      </Grid>
      <CenteredGrid item lg={6}>
        <Pagination total={totalResults} page={page} first={first} setPage={setPage} setSkip={setSkip} />
      </CenteredGrid>
      <RightAlignedGrid container item lg={3} alignItems="center" justify="flex-end">
        <Grid item>
          {!isOnBottom && <JumpToSelect label="Go to" goToOptions={goToOptions} />}
          <NumberOfItemsSelect first={first} setFirst={setFirst} label={typeLabel} />
        </Grid>
        <Grid item>{settingGroups.length > 0 && <SettingsPanel panelId="cardGallerySettings" settingGroups={settingGroups} />}</Grid>
      </RightAlignedGrid>
      {isOnBottom && (
        <Typography
          variant="body2"
          color="primary"
          onClick={() => window?.scrollTo({ top: 0, left: 0, behavior: 'smooth' })}
          style={{ cursor: 'pointer', display: 'inline' }}
        >
          (Back to Top)
        </Typography>
      )}
    </StyledDesktopOnlyGrid>
  );
};

const StyledDesktopOnlyGrid = styled(Grid)(({ theme }) => ({
  [theme.breakpoints.down('md')]: {
    display: 'none',
  },
}));

const GalleryPaginationMobile: React.FC<GalleryPaginationDesktopProps> = ({
  startOfRange,
  endOfRange,
  totalResults,
  typeLabel,
  galleryType,
  settingGroups,
  page,
  first,
  setFirst,
  setPage,
  setSkip,
  isOnBottom = false,
  isFetching = true,
  goToOptions = [],
}) => {
  const [previousStartOfRange, setPreviousStartOfRange] = useState(startOfRange);
  const [previousEndOfRange, setPreviousEndOfRange] = useState(endOfRange);
  const [previousTotalResults, setPreviousTotalResults] = useState(totalResults);

  const rangeValuesAreUpdatingViaPagination =
    totalResults === previousTotalResults && (startOfRange !== previousStartOfRange || endOfRange !== previousEndOfRange);
  const rangeValuesAreValid = startOfRange <= endOfRange && endOfRange <= totalResults;

  useEffect(() => {
    if (!isFetching && rangeValuesAreValid) {
      setPreviousStartOfRange(startOfRange);
      setPreviousEndOfRange(endOfRange);
      setPreviousTotalResults(totalResults);
    }
  }, [isFetching, startOfRange, endOfRange, totalResults]);

  return (
    <StyledMobileOnlyGrid
      container
      spacing={0}
      justify="center"
      alignItems="center"
      style={{ marginBottom: galleryType === 'sets' ? '10px' : '0px' }}
    >
      <StyledGridCenteredIfSmall item sm={3} xs={12}>
        {isFetching && rangeValuesAreUpdatingViaPagination && rangeValuesAreValid && (
          <Typography>{`Showing ${previousStartOfRange}-${previousEndOfRange} of ${previousTotalResults} ${typeLabel.toLowerCase()}`}</Typography>
        )}
        {isFetching && !rangeValuesAreValid && (
          <Typography
            style={{ visibility: 'hidden' }}
          >{`Showing ${previousStartOfRange}-${previousEndOfRange} of ${previousTotalResults} ${typeLabel.toLowerCase()}`}</Typography>
        )}
        {!isFetching && rangeValuesAreValid && (
          <Typography>{`Showing ${startOfRange}-${endOfRange} of ${totalResults} ${typeLabel.toLowerCase()}`}</Typography>
        )}
      </StyledGridCenteredIfSmall>
      <StyledGridLeftAlignedIfSmall item sm={6} xs={9}>
        <Pagination total={totalResults} page={page} first={first} setPage={setPage} setSkip={setSkip} />
      </StyledGridLeftAlignedIfSmall>
      <RightAlignedGrid container item sm={3} xs={3} alignItems="center" justify="flex-end">
        <Grid item>
          <NumberOfItemsSelect first={first} setFirst={setFirst} label={typeLabel} />
        </Grid>
        <Grid item>{settingGroups.length > 0 && <SettingsPanel panelId="cardGallerySettings" settingGroups={settingGroups} />}</Grid>
      </RightAlignedGrid>
      <RightAlignedGrid container item sm={12} xs={12} alignItems="center" justify="center">
        <Grid item style={{ marginTop: '10px', marginBottom: '10px' }}>
          {!isOnBottom && <JumpToSelect label="Go to" goToOptions={goToOptions} isMobile />}
        </Grid>
      </RightAlignedGrid>
      {isOnBottom && (
        <Typography
          variant="body2"
          color="primary"
          onClick={() => window?.scrollTo({ top: 0, left: 0, behavior: 'smooth' })}
          style={{ cursor: 'pointer', display: 'inline', marginLeft: '5px' }}
        >
          (Back to Top)
        </Typography>
      )}
    </StyledMobileOnlyGrid>
  );
};

const StyledMobileOnlyGrid = styled(Grid)(({ theme }) => ({
  [theme.breakpoints.up('lg')]: {
    display: 'none',
  },
}));

const StyledGridCenteredIfSmall = styled(Grid)(({ theme }) => ({
  textAlign: 'left',
  [theme.breakpoints.down('xs')]: {
    textAlign: 'center',
  },
}));

const StyledGridLeftAlignedIfSmall = styled(Grid)(({ theme }) => ({
  textAlign: 'center',
  [theme.breakpoints.down('xs')]: {
    textAlign: 'left',
  },
}));

export default GalleryControls;
