import CircularProgress from '@material-ui/core/CircularProgress';
import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import CheckCircleIcon from '@material-ui/icons/CheckCircle';
import { useSnackbar } from 'notistack';
import { useEffect, useState } from 'react';
import { useUpdateCollectionLegacyMutation } from '../../network/services/mtgcbApi';
import useDebouncedCallback, { cardQuantityInputFieldDebounceTimeMs } from '../../util/useDebouncedCallback';

interface CardQuantitySelectorProps {
  cardId: number;
  cardName?: string;
  quantityReg?: number | '';
  quantityFoil?: number | '';
  userId: string;
  setId: string;
  renderNormal?: boolean;
  renderFoil?: boolean;
}

const CardQuantitySelector: React.FC<CardQuantitySelectorProps> = ({
  cardId,
  cardName,
  quantityReg = 0,
  quantityFoil = 0,
  userId = null,
  setId = null,
  renderNormal = true,
  renderFoil = true,
}) => {
  const [isLoadingSlowly, setIsLoadingSlowly] = useState(false);

  const [internalQuantityReg, setInternalQuantityReg] = useState(quantityReg);
  const [internalQuantityFoil, setInternalQuantityFoil] = useState(quantityFoil);

  const [updateCollectionRegular, { isLoading, isSuccess, isError }] = useUpdateCollectionLegacyMutation({});
  const updateCollectionRegularDebounced = useDebouncedCallback(updateCollectionRegular, cardQuantityInputFieldDebounceTimeMs);

  const [
    updateCollectionFoil,
    { isLoading: isLoadingFoil, isSuccess: isSuccessFoil, isError: isErrorFoil },
  ] = useUpdateCollectionLegacyMutation({});
  const updateCollectionFoilDebounced = useDebouncedCallback(updateCollectionFoil, cardQuantityInputFieldDebounceTimeMs);

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

  const { enqueueSnackbar } = useSnackbar();

  useEffect(() => {
    if (isSuccess) {
      const newQuantityReg = internalQuantityReg;
      enqueueSnackbar(`${cardName ?? 'Card'} has been set to ${newQuantityReg}.`, {
        variant: 'success',
        anchorOrigin: { horizontal: 'right', vertical: 'bottom' },
      });
    }
  }, [isSuccess]);

  useEffect(() => {
    if (isSuccessFoil) {
      const newQuantityFoil = internalQuantityFoil;
      enqueueSnackbar(`${cardName ?? 'Card'} has been set to ${newQuantityFoil} foil(s).`, {
        variant: 'success',
        anchorOrigin: { horizontal: 'right', vertical: 'bottom' },
      });
    }
  }, [isSuccessFoil]);

  useEffect(() => {
    if (isError) {
      enqueueSnackbar(`${cardName ?? 'Card'} could not be updated.`, {
        variant: 'error',
        anchorOrigin: { horizontal: 'right', vertical: 'bottom' },
      });
    }
  }, [isError]);

  useEffect(() => {
    if (isErrorFoil) {
      enqueueSnackbar(`${cardName ?? 'Card'} could not be updated.`, {
        variant: 'error',
        anchorOrigin: { horizontal: 'right', vertical: 'bottom' },
      });
    }
  }, [isErrorFoil]);

  const handleQuantityRegChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    if (event.target.value === '') {
      setInternalQuantityReg('');
      return;
    }

    let newQuantityReg = parseInt(event.target.value, 10);

    if (newQuantityReg < 0) {
      newQuantityReg = 0;
    }
    setInternalQuantityReg(newQuantityReg);

    updateCollectionRegularDebounced({
      cardId,
      quantityRegular: newQuantityReg,
      mode: 'set',
      setId,
      userId,
    });
  };

  const handleQuantityFoilChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    if (event.target.value === '') {
      setInternalQuantityFoil('');
      return;
    }

    let newQuantityFoil = parseInt(event.target.value, 10);

    if (newQuantityFoil < 0) {
      newQuantityFoil = 0;
    }
    setInternalQuantityFoil(newQuantityFoil);

    updateCollectionFoilDebounced({
      cardId,
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
                  {isLoading && isLoadingSlowly && <CircularProgress size={10} variant="indeterminate" />}
                  {isSuccess && <CheckCircleIcon style={{ fontSize: '12px' }} />}
                </>
              </>
            }
            value={internalQuantityReg}
            onChange={handleQuantityRegChange}
            onBlur={() => {
              if (internalQuantityReg === '') {
                setInternalQuantityReg(0);
                updateCollectionRegularDebounced({
                  cardId,
                  quantityRegular: 0,
                  mode: 'set',
                  setId,
                  userId,
                });
              }
            }}
            InputLabelProps={{
              shrink: true,
            }}
            margin="dense"
            variant="outlined"
            fullWidth
            error={isError}
            helperText={isError && 'Error -- please try again'}
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
                  {isLoadingFoil && isLoadingSlowly && <CircularProgress size={10} variant="indeterminate" />}
                  {isSuccessFoil && <CheckCircleIcon style={{ fontSize: '12px' }} />}
                </>
              </>
            }
            value={internalQuantityFoil}
            onChange={handleQuantityFoilChange}
            onBlur={() => {
              if (internalQuantityFoil === '') {
                setInternalQuantityFoil(0);
                updateCollectionFoilDebounced({
                  cardId,
                  quantityFoil: 0,
                  mode: 'set',
                  setId,
                  userId,
                });
              }
            }}
            InputLabelProps={{
              shrink: true,
            }}
            margin="dense"
            variant="outlined"
            fullWidth
            error={isErrorFoil}
            helperText={isErrorFoil && 'Error -- please try again'}
          />
        </Grid>
      )}
    </Grid>
  );
};
export default CardQuantitySelector;
