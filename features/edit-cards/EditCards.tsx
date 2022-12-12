import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import Typography from '@material-ui/core/Typography';
import Autocomplete from '@material-ui/lab/Autocomplete';
import { useRouter } from 'next/router';
import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import {
  useGetCardAutocompleteQuery,
  useGetCollectionByCardIdLegacyQuery,
  useUpdateCollectionLegacyMutation,
} from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import { useDebounce, useFocus } from '../../util';
import { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import useWindowDimensions from '../../util/useWindowDimensions';
import CardBox from '../browse/CardBox';
import { CardQuantitySubmitter } from './CardQuantitySubmitter';
import { setFormVisibility } from './editCardsSlice';

const EditCards: React.FC = () => {
  const router = useRouter();
  const dispatch = useDispatch();
  const { isAuthenticated, isCheckingAuth, user } = useAuthentication();
  const [autocompleteRef, setAutocompleteFocus] = useFocus();

  const { height } = useWindowDimensions();
  const cardHeight = Math.min(330 + (height - 700), 830);

  const { priceType, cardEditingMode } = useSelector((state: RootState) => state.editCards);

  const [options, setOptions] = useState<Options[]>([]);
  const [search, setSearch] = useState('');
  const debouncedSearch = useDebounce(search, searchFieldDebounceTimeMs);

  const [selectedCard, setSelectedCard] = useState<Options>(null);
  const [cardIds, setCardIds] = useState<number[]>([]);

  const { data, isLoading, error } = useGetCardAutocompleteQuery(
    { name: debouncedSearch },
    { skip: !debouncedSearch, refetchOnMountOrArgChange: true }
  );

  const [
    updateCollection,
    { isLoading: isUpdatingCollection, isSuccess: hasUpdatedCollection, isError: updatingCollectionError, reset: resetUpdateCollection },
  ] = useUpdateCollectionLegacyMutation({});

  const {
    data: collectionByCardIdResponse,
    isFetching: isCollectionByCardIdFetching,
    error: collectionByCardIdError,
  } = useGetCollectionByCardIdLegacyQuery(
    {
      userId: user?.id,
      cardIds,
    },
    { skip: user?.id == null || cardIds == null || cardIds?.length === 0 }
  );

  const quantityReg = collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.[0]?.quantityReg || 0;
  const quantityFoil = collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.[0]?.quantityFoil || 0;

  const handleUpdateCollection = (newQuantityReg, newQuantityFoil, mode) => {
    updateCollection({
      cardId: selectedCard.id,
      quantityRegular: newQuantityReg,
      quantityFoil: newQuantityFoil,
      mode,
      userId: user?.id,
      setId: selectedCard.set.id,
    });
  };

  const handleUpdateSuccess = () => {
    setSelectedCard(null);
    setSearch('');
    setOptions([]);
    setCardIds([]);
    resetUpdateCollection();
    setAutocompleteFocus();
  };

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  useEffect(() => {
    if (data?.data?.cards) {
      setOptions(data.data.cards);
    } else {
      setOptions([]);
      setSelectedCard(null);
    }
  }, [data]);

  useEffect(() => {
    if (search === '') {
      setOptions([]);
      setSelectedCard(null);
    }
  }, [search]);

  useEffect(() => {
    if (selectedCard) {
      setCardIds([selectedCard.id]);
    } else {
      setCardIds([]);
    }
  }, [selectedCard]);

  if (isCheckingAuth) {
    return <></>;
  }

  if (!isAuthenticated) {
    router.push('/login');
    return <></>;
  }
  return (
    <ResponsiveContainer maxWidth="xl">
      <Typography variant="h4" component="h1" align="center">
        Edit Cards
      </Typography>
      <Grid container justify="center" style={{ marginTop: '10px' }} spacing={0}>
        <Grid item xs={12} sm={12} md={6} lg={4}>
          <Autocomplete
            renderInput={(params) => (
              <TextField
                autoFocus
                {...params}
                label="Search for a card to edit!"
                placeholder="Ex. Giant Spider"
                variant="outlined"
                onChange={(e) => setSearch(e.target.value)}
                value={search}
                inputRef={autocompleteRef}
              />
            )}
            options={options}
            getOptionLabel={(card) => `${card?.name} [${card?.set?.name}]`}
            onChange={(e, newSelectedCard) => {
              setSelectedCard(newSelectedCard as Options);
            }}
            noOptionsText={search ? 'No cards found -- try another search' : 'Start typing to search for a card'}
            loadingText="Searching..."
            loading={isLoading}
            value={selectedCard}
          />
        </Grid>
      </Grid>
      <Grid container justify="center" style={{ marginTop: '10px' }}>
        {selectedCard && (
          <>
            <Grid item xs={12}>
              <div style={{ textAlign: 'center' }}>
                <CardBox
                  card={selectedCard}
                  nameIsVisible={false}
                  setIsVisible={false}
                  priceType={priceType}
                  fixedHeight={`${cardHeight}px`}
                />
              </div>
            </Grid>
            <Grid item xs={12} sm={6} md={6} lg={4}>
              {collectionByCardIdResponse && !isCollectionByCardIdFetching && (
                <CardQuantitySubmitter
                  mode={cardEditingMode}
                  cardName={selectedCard?.name}
                  quantityReg={quantityReg}
                  quantityFoil={quantityFoil}
                  handleUpdateCollection={handleUpdateCollection}
                  handleUpdateSuccess={handleUpdateSuccess}
                  isLoading={isUpdatingCollection}
                  isSuccess={hasUpdatedCollection}
                  isError={updatingCollectionError}
                />
              )}
            </Grid>
          </>
        )}
      </Grid>
    </ResponsiveContainer>
  );
};

export default EditCards;

interface Options {
  name: string;
  set: {
    id: string;
    name: string;
    slug: string;
  };
  id: number;
  low: number;
  average: number;
  high: number;
  market: number;
  foil: number;
  tcgplayerId: number;
}
