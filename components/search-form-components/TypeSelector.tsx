import TextField from '@material-ui/core/TextField';
import isMobile from 'is-mobile';
import styled from 'styled-components';
import { CardType } from '../../features/browse/browseSlice';
import { MappedCardTypes } from '../../features/browse/forms/mappers/mapCardTypes';
import AutocompleteWithNegation from '../AutocompleteWithNegation';

interface TypeSelectorProps {
  types: MappedCardTypes[];
  cardTypes: CardType[];
  updateCardTypes: (cardTypes: CardType[]) => void;
}

const NativeTypeSelector: React.FC<TypeSelectorProps> = ({ types, cardTypes, updateCardTypes }) => {
  const typesByCategory = types.reduce((acc, type) => {
    if (!acc[type.category]) {
      acc[type.category] = [];
    }
    acc[type.category].push(type);
    return acc;
  }, {} as { [category: string]: MappedCardTypes[] });

  const value = cardTypes.map((type) => type.value);

  return (
    <TextField
      id="native-type-selector"
      select
      SelectProps={{ native: true, multiple: true, variant: 'outlined', label: 'Card Types' }}
      InputLabelProps={{
        shrink: true,
      }}
      style={{
        padding: 0,
        margin: 0,
        width: '100%',
      }}
      label="Card Types"
      value={value}
      variant="outlined"
      onChange={(e) => {
        const { options } = (e.target as unknown) as HTMLSelectElement;
        const selectedCardTypes: CardType[] = [];

        for (let i = 0, l = options.length; i < l; i += 1) {
          if (options[i].selected) {
            selectedCardTypes.push({
              category: types.find((type) => type.value === options[i].value).category,
              label: types.find((type) => type.value === options[i].value).label,
              value: options[i].value as string,
              exclude: false,
            });
          }
        }
        updateCardTypes(selectedCardTypes as CardType[]);
      }}
    >
      {Object.keys(typesByCategory).map((category) => (
        <optgroup key={category} label={category}>
          {typesByCategory[category].map((type) => (
            <option key={type.value} value={type.value}>
              {type.label}
            </option>
          ))}
        </optgroup>
      ))}
    </TextField>
  );
};

const TypeSelector: React.FC<TypeSelectorProps> = ({ types, cardTypes, updateCardTypes }) => {
  const isMobileBrowser = isMobile();

  return (
    <>
      {types && !isMobileBrowser && (
        <StyledTypeSelector>
          <AutocompleteWithNegation
            label="Card Types"
            options={types}
            selectedOptions={cardTypes}
            setSelectedOptionsRemotely={updateCardTypes}
          />
        </StyledTypeSelector>
      )}
      {types && isMobileBrowser && (
        <StyledNativeTypeSelector>
          <NativeTypeSelector types={types} cardTypes={cardTypes} updateCardTypes={updateCardTypes} />
        </StyledNativeTypeSelector>
      )}
    </>
  );
};

const StyledTypeSelector = styled.div({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
});

const StyledNativeTypeSelector = styled.div({
  textAlign: 'center',
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
  width: '100%',
});

export default TypeSelector;
