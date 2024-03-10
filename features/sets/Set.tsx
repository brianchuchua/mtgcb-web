import Divider from '@material-ui/core/Divider';
import Typography from '@material-ui/core/Typography';
import { Skeleton } from '@material-ui/lab';
import { memo, useEffect } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Element } from 'react-scroll';
import styled from 'styled-components';
import Breadcrumbs from '../../components/layout/Breadcrumbs';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import { useGetSetBySlugQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import { determineSetLabel } from '../../util';
import CardGallery from '../browse/CardGallery';
import CardTable from '../browse/CardTable';
import { useCardSearch } from '../browse/hooks/useCardSearch';
import titleCase from '../browse/util/titleCase';
import { useGetSubsetsByGroupId } from './hooks/useGetSubsetsByGroupId';
import { useGetSubsetsByParentSetId } from './hooks/useGetSubsetsByParentSetId';
import { setCardSets, setFormVisibility, setSubsets } from './setSlice';
import { Subset } from './Subset';

interface SetProps {
  setSlug: string;
}

export const Set: React.FC<SetProps> = ({ setSlug }) => {
  const reduxSlice = 'set';

  const dispatch = useDispatch();

  const { viewSubject, viewMode, priceType } = useSelector((state: RootState) => state[reduxSlice]);

  const {
    cards,
    isCardDataLoading,
    isCardDataFetching,
    isCardMetaDataLoading,
    isCardMetaDataFetching,
    skip,
    setSkip,
    page,
    setPage,
    first,
    setFirst,
    totalResults,
  } = useCardSearch(reduxSlice, setFormVisibility, false);

  const { data: setData, isLoading: isSetLoading, isFetching: isSetFetching, error: setError } = useGetSetBySlugQuery({ slug: setSlug });
  const set = setData?.data?.sets?.[0];

  useEffect(() => {
    dispatch(
      setCardSets([
        {
          category: 'Sets',
          value: setData?.data?.sets?.[0]?.id,
          label: setData?.data?.sets?.[0]?.name,
          exclude: false,
        },
      ])
    );
  }, [setData?.data?.sets?.[0]?.id, setData?.data?.sets?.[0]?.name]);

  const { subsetByGroupIdOptions } = useGetSubsetsByGroupId(set?.id);
  const { subsets, goToOptions } = useGetSubsetsByParentSetId(set?.id, set?.isSubsetGroup);

  const isLoading = isSetLoading || isCardDataLoading || isCardMetaDataLoading;
  const isFetching = isSetFetching || isCardDataFetching || isCardMetaDataFetching;

  // TODO: Make a nice set icon component with intelligent fallbacks or a default option
  // TODO: Add buy links here and come up with a good interface, similar to how Scryfall does card pages perhaps
  // TODO: Add charts/analysis/something cool here

  const setLabel = determineSetLabel(set);

  return (
    <ResponsiveContainer maxWidth="xl" id="set-container">
      <div>
        {set && (
          <div>
            {(isLoading || setSlug !== setData?.data?.sets?.[0].slug) && (
              <CenteredSkeleton variant="rect" width="100%">
                <div style={{ textAlign: 'center' }}>
                  <Typography variant="h4" component="div">
                    {set?.name}
                  </Typography>
                  <i
                    className={`ss ss-${set.code.toLowerCase()} ss-5x ss-common ss-fw`}
                    style={{
                      // WebkitTextStroke: '1px #fff', // TODO: Use this style for a complete set so I can support ss-mythic ss-grad
                      textShadow: '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff',
                      paddingBottom: '5px',
                    }}
                  />
                  <Typography variant="body2" color="textSecondary" component="div">
                    {set.releasedAt?.slice(0, 10)}
                  </Typography>
                  <Typography variant="body2" color="textSecondary" component="div">
                    {set.cardCount ? `${set.cardCount} cards` : ''}
                  </Typography>
                  <Typography variant="body2" color="textSecondary" component="div">
                    {set.category} {setLabel}
                    {set.setType ? ` - ${titleCase(set.setType)}` : ''}
                  </Typography>
                </div>
              </CenteredSkeleton>
            )}
            {!isLoading && (
              <div style={{ textAlign: 'center' }}>
                <Breadcrumbs
                  links={[
                    {
                      title: 'Browse',
                      url: '/browse',
                    },
                    {
                      title: set?.name,
                      url: set?.slug ? `/browse/sets/${set?.slug}` : '',
                    },
                  ]}
                />
                <Element name={`anchor-link-${set?.slug}`} />
                <Typography variant="h4" component="div" id={`anchor-link-${set?.slug}`}>
                  {set?.name}
                </Typography>
                <i
                  className={`ss ss-${set.code.toLowerCase()} ss-5x ss-common ss-fw`}
                  style={{
                    // WebkitTextStroke: '1px #fff', // TODO: Use this style for a complete set so I can support ss-mythic ss-grad
                    textShadow: '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff',
                    paddingBottom: '5px',
                  }}
                />
                <Typography variant="body2" color="textSecondary" component="div">
                  {set.releasedAt?.slice(0, 10)}
                </Typography>
                <Typography variant="body2" color="textSecondary" component="div">
                  {set.cardCount ? `${set.cardCount} cards` : ''}
                </Typography>
                <Typography variant="body2" color="textSecondary" component="div">
                  {set.category} {setLabel}
                  {set.setType ? ` - ${titleCase(set.setType)}` : ''}
                </Typography>
              </div>
            )}
            {viewSubject === 'cards' && viewMode === 'grid' && (
              <MemoizedCardGallery
                cards={cards}
                totalResults={totalResults}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
                priceType={priceType}
                isLoading={isLoading}
                isFetching={isFetching}
                goToOptions={goToOptions}
                subsetOptions={subsetByGroupIdOptions}
                showSubsetFilter={set?.isSubsetGroup}
                setSubsets={setSubsets}
                reduxSlice={reduxSlice}
              />
            )}
            {viewSubject === 'cards' && viewMode === 'grid' && subsets?.length > 0 && !set?.isSubsetGroup && (
              <>
                {subsets.map((subset) => (
                  <div key={`subset-grid-${subset.id}`}>
                    <Divider style={{ marginTop: '15px', marginBottom: '15px' }} />
                    <Subset key={subset.id} setSlug={subset.slug} />
                  </div>
                ))}
              </>
            )}
            {viewSubject === 'cards' && viewMode === 'table' && (
              <MemoizedCardTable
                cards={cards}
                totalResults={totalResults}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
                priceType={priceType}
                isShowingSingleSet
                isFetching={isFetching}
                isLoading={isLoading}
                goToOptions={goToOptions}
                subsetOptions={subsetByGroupIdOptions}
                showSubsetFilter={set?.isSubsetGroup}
                setSubsets={setSubsets}
                reduxSlice={reduxSlice}
              />
            )}
            {viewSubject === 'cards' && viewMode === 'table' && subsets?.length > 0 && !set?.isSubsetGroup && (
              <>
                {subsets.map((subset) => (
                  <div key={`subset-table-${subset.id}`}>
                    <Divider style={{ marginTop: '15px', marginBottom: '15px' }} />
                    <Subset key={subset.id} setSlug={subset.slug} />
                  </div>
                ))}
              </>
            )}
          </div>
        )}
        {((setData?.data?.sets && setData?.data?.sets.length === 0) || setError) && <p>No set found</p>}
      </div>
    </ResponsiveContainer>
  );
};

const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);

const CenteredSkeleton = styled(Skeleton)({
  margin: '0 auto',
});
