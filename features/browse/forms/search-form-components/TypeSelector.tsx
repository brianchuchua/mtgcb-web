import { useEffect, useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { getCardTypes } from '../../../../network/features/browse';
import { CardType, setCardTypes } from '../../browseSlice';
import { mapCardTypes } from '../mappers';

const TypeSelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateCardTypes = (newCardTypes: CardType[]) => {
    dispatch(setCardTypes({ cardTypes: newCardTypes }));
  };

  const [types, setTypes] = useState(null);
  const [selectedTypes, setSelectedTypes] = useState([]);

  useEffect(() => {
    async function fetchTypes() {
      const cardTypesResponse = await getCardTypes();
      const allCardTypes = cardTypesResponse?.data?.data?.cardTypes;
      const allCardTypesMapped = mapCardTypes(allCardTypes);

      setTypes(allCardTypesMapped);
    }
    fetchTypes();
  }, []);

  return (
    types && (
      <StyledTypeSelector>
        <AutocompleteWithNegation
          label="Card Types"
          options={types}
          selectedOptions={selectedTypes}
          setSelectedOptionsLocally={setSelectedTypes}
          setSelectedOptionsRemotely={updateCardTypes}
        />
      </StyledTypeSelector>
    )
  );
};

const StyledTypeSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default TypeSelector;
