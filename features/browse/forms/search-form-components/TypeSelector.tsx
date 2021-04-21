import { useEffect, useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import Autocomplete from '@material-ui/lab/Autocomplete';
import Chip from '@material-ui/core/Chip';
import TextField from '@material-ui/core/TextField';
import Tooltip from '@material-ui/core/Tooltip';
import Zoom from '@material-ui/core/Zoom';
import { setCardTypes, CardType } from '../../browseSlice';
import { getCardTypes } from '../../../../network/features/browse';
import mapCardTypes from '../mappers/mapCardTypes';

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
        <Autocomplete
          multiple
          filterSelectedOptions
          autoComplete
          options={types}
          groupBy={(type) => type.categoryLabel}
          getOptionLabel={(type) => type.cardType}
          getOptionSelected={(option, value) => option.cardType === value.cardType}
          renderInput={(params) => <TextField {...params} label="Card Types" variant="outlined" />}
          onChange={(e, newSelectedTypes) => {
            setSelectedTypes(newSelectedTypes);
            updateCardTypes(newSelectedTypes);
          }}
          value={selectedTypes}
          renderTags={(value: CardType[], getTagProps) =>
            value.map((option: CardType, index: number) => (
              <Tooltip
                key={option.cardType}
                TransitionComponent={Zoom}
                title={option.exclude ? '(Click to include this type)' : '(Click to exclude this type)'}
              >
                <Chip
                  label={option.cardType}
                  {...getTagProps({ index })}
                  color={option.exclude ? 'default' : 'primary'}
                  clickable
                  onClick={() => {
                    const clickedCardType = { ...selectedTypes[index] };
                    clickedCardType.exclude = !clickedCardType.exclude;

                    const updatedTypes = [...selectedTypes];
                    updatedTypes[index] = clickedCardType;

                    setSelectedTypes(updatedTypes);
                    updateCardTypes(updatedTypes);
                  }}
                  style={{ textDecoration: option.exclude ? 'line-through' : '' }}
                />
              </Tooltip>
            ))
          }
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
