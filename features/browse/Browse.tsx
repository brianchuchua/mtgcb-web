import { useState, useEffect } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import Container from '@material-ui/core/Container';
import Link from '../../components/Link';
import { RootState } from '../../redux/rootReducer';
import { setFormVisibility } from './browseSlice';
import { getAllCards, getAllCardsMeta } from '../../network/features/browse';
import CardGallery from './CardGallery';
import { useDebouncedEffect } from '../../util';
import { determineSortFilter } from '../../network/features/browse/filters';

export const Browse: React.FC = () => {
  const {
    searchQuery,
    oracleTextQuery,
    cardTypes,
    cardSets,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
  } = useSelector((state: RootState) => state.browse);
  const [cards, setCards] = useState([]);
  const [totalResults, setTotalResults] = useState(0);
  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useState(50);
  const [page, setPage] = useState(1);
  const [previousQuery, setPreviousQuery] = useState('');
  const [previousOracleTextQuery, setPreviousOracleTextQuery] = useState('');
  const [previousCardTypes, setPreviousCardTypes] = useState([]);
  const [previousCardSets, setPreviousCardSets] = useState([]);
  const [previousCardRarities, setPreviousCardRarities] = useState([]);
  const [previousColors, setPreviousColors] = useState({});
  const [previousShowAllPrintings, setPreviousShowAllPrintings] = useState(true);
  const [previousCardStatSearches, setPreviousCardStatSearches] = useState([]);
  const [previousSortBy, setPreviousSortBy] = useState('name');
  const [previousSortByDirection, setPreviousSortByDirection] = useState('ASC');

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, [dispatch]);

  useDebouncedEffect(
    async () => {
      async function fetchCards() {
        const cardFilterChanged =
          previousQuery !== searchQuery ||
          previousCardTypes !== cardTypes ||
          previousCardSets !== cardSets ||
          previousCardRarities !== cardRarities ||
          previousOracleTextQuery !== oracleTextQuery ||
          previousColors !== cardColors ||
          previousShowAllPrintings !== showAllPrintings ||
          previousCardStatSearches !== cardStatSearches ||
          previousSortBy !== sortBy ||
          previousSortByDirection !== sortByDirection;

        const currentPage = cardFilterChanged ? 1 : page;
        setPage(currentPage);
        setSkip((currentPage - 1) * first);

        setPreviousQuery(searchQuery);
        setPreviousOracleTextQuery(oracleTextQuery);
        setPreviousCardTypes(cardTypes);
        setPreviousCardSets(cardSets);
        setPreviousCardRarities(cardRarities);
        setPreviousColors(cardColors);
        setPreviousShowAllPrintings(showAllPrintings);
        setPreviousCardStatSearches(cardStatSearches);
        setPreviousSortBy(sortBy);
        setPreviousSortByDirection(sortByDirection);

        const allCardsResponse = await getAllCards({
          name: searchQuery,
          oracleTextQuery,
          first,
          skip,
          cardTypes,
          cardSets,
          cardRarities,
          cardColors,
          showAllPrintings,
          cardStatSearches,
          sortBy: determineSortFilter(sortBy, sortByDirection),
        });
        const allCards = allCardsResponse?.data?.data?.allCards;
        setCards(allCards);

        const allCardsMetaResponse = await getAllCardsMeta({
          name: searchQuery,
          oracleTextQuery,
          first,
          skip,
          cardTypes,
          cardSets,
          cardRarities,
          cardColors,
          showAllPrintings,
          cardStatSearches,
          sortBy: determineSortFilter(sortBy, sortByDirection),
        });
        const count = allCardsMetaResponse?.data?.data?._allCardsMeta?.count || 0;
        setTotalResults(count);
      }
      fetchCards();
    },
    400,
    [
      searchQuery,
      oracleTextQuery,
      cardTypes,
      cardSets,
      cardRarities,
      cardColors,
      showAllPrintings,
      cardStatSearches,
      skip,
      first,
      sortBy,
      sortByDirection,
    ]
  );

  // TODO: Make better Breadcrumbs component
  return (
    <Container maxWidth="xl">
      <Breadcrumbs separator=">" aria-label="breadcrumb">
        <Link href="/" variant="body2" color="inherit">
          MTG CB
        </Link>
        <Link href="#" variant="body2" color="inherit">
          Browse
        </Link>
      </Breadcrumbs>
      <ContentWrapper>
        <CardGallery
          cards={cards}
          totalResults={totalResults}
          first={first}
          skip={skip}
          page={page}
          setSkip={setSkip}
          setFirst={setFirst}
          setPage={setPage}
        />
      </ContentWrapper>
    </Container>
  );
};

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));
