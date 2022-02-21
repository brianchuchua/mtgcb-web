import CircularProgress from '@material-ui/core/CircularProgress';
import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import CheckCircleIcon from '@material-ui/icons/CheckCircle';
import { useEffect, useState } from 'react';
import { useUpdateCollectionLegacyMutation } from '../../network/services/mtgcbApi';
import useDebouncedCallback, { cardQuantityInputFieldDebounceTimeMs } from '../../util/useDebouncedCallback';

interface CardQuantitySelectorProps {
  cardId: number;
  quantityReg?: number;
  quantityFoil?: number;
  userId: string;
  setId: string;
  renderNormal?: boolean;
  renderFoil?: boolean;
}

const CardQuantitySelector: React.FC<CardQuantitySelectorProps> = ({
  cardId,
  quantityReg = 0,
  quantityFoil = 0,
  userId = null,
  setId = null,
  renderNormal = true,
  renderFoil = true,
}) => {
  const [isUpdatingNormal, setIsUpdatingNormal] = useState(false);
  const [isUpdatingFoil, setIsUpdatingFoil] = useState(false);
  const [isLoadingSlowly, setIsLoadingSlowly] = useState(false);
  const [wasUpdatingNormal, setWasUpdatingNormal] = useState(false);
  const [wasUpdatingFoil, setWasUpdatingFoil] = useState(false);

  const [internalQuantityReg, setInternalQuantityReg] = useState(quantityReg);
  const [internalQuantityFoil, setInternalQuantityFoil] = useState(quantityFoil);

  const [updateCollection, { isLoading, isSuccess, isError }] = useUpdateCollectionLegacyMutation();

  const updateCollectionDebounced = useDebouncedCallback(updateCollection, cardQuantityInputFieldDebounceTimeMs);

  useEffect(() => {
    setInternalQuantityReg(quantityReg);
    setInternalQuantityFoil(quantityFoil);
  }, [quantityReg, quantityFoil]);

  useEffect(() => {
    if (isLoading) {
      setTimeout(() => {
        setIsLoadingSlowly(true);
      }, 500);
    } else {
      setIsLoadingSlowly(false);
    }
  }, [isLoading]);

  useEffect(() => {
    if (isSuccess) {
      if (isUpdatingNormal) {
        setIsUpdatingNormal(false);
        setWasUpdatingNormal(true);
        setTimeout(() => {
          setWasUpdatingNormal(false);
        }, 3000);
      }
      if (isUpdatingFoil) {
        setIsUpdatingFoil(false);
        setWasUpdatingFoil(true);
        setTimeout(() => {
          setWasUpdatingFoil(false);
        }, 3000);
      }
    }
  }, [isSuccess]);

  const handleQuantityRegChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    let newQuantityReg = parseInt(event.target.value, 10);
    if (newQuantityReg < 0) {
      newQuantityReg = 0;
    }
    setInternalQuantityReg(newQuantityReg);
    setWasUpdatingNormal(false);
    setIsUpdatingNormal(true);

    updateCollectionDebounced({
      cardId,
      quantityRegular: newQuantityReg,
      quantityFoil,
      mode: 'set',
      setId,
      userId,
    });
  };

  const handleQuantityFoilChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    let newQuantityFoil = parseInt(event.target.value, 10);
    if (newQuantityFoil < 0) {
      newQuantityFoil = 0;
    }
    setInternalQuantityFoil(newQuantityFoil);
    setWasUpdatingFoil(false);
    setIsUpdatingFoil(true);

    updateCollectionDebounced({
      cardId,
      quantityRegular: quantityReg,
      quantityFoil: newQuantityFoil,
      mode: 'set',
      setId,
      userId,
    });
  };

  return (
    <Grid container spacing={1}>
      {renderNormal && (
        <Grid item xs={renderNormal && renderFoil ? 6 : 12}>
          <TextField
            type="number"
            label={
              <>
                <>Regular</>{' '}
                <>
                  {isLoading && isUpdatingNormal && isLoadingSlowly && <CircularProgress size={10} variant="indeterminate" />}
                  {isSuccess && wasUpdatingNormal && <CheckCircleIcon style={{ fontSize: '12px' }} />}
                </>
              </>
            }
            value={internalQuantityReg}
            onChange={handleQuantityRegChange}
            InputLabelProps={{
              shrink: true,
            }}
            margin="dense"
            variant="outlined"
            fullWidth
            error={isError && isUpdatingNormal}
            helperText={isError && isUpdatingNormal && 'Error -- please try again'}
          />
        </Grid>
      )}
      {renderFoil && (
        <Grid item xs={renderNormal && renderFoil ? 6 : 12}>
          <TextField
            type="number"
            label={
              <>
                <>Foils</>{' '}
                <>
                  {isLoading && isUpdatingFoil && isLoadingSlowly && <CircularProgress size={10} variant="indeterminate" />}
                  {isSuccess && wasUpdatingFoil && <CheckCircleIcon style={{ fontSize: '12px' }} />}
                </>
              </>
            }
            value={internalQuantityFoil}
            onChange={handleQuantityFoilChange}
            InputLabelProps={{
              shrink: true,
            }}
            margin="dense"
            variant="outlined"
            fullWidth
            error={isError && isUpdatingFoil}
            helperText={isError && isUpdatingFoil && 'Error -- please try again'}
          />
        </Grid>
      )}
    </Grid>
  );
};
export default CardQuantitySelector;
